module Database.QUDB.Structure (new, load, dump, query, DB) where

import Database.QUDB.EntityTypes
import Database.QUDB.Query
import Data.List (elemIndex, sortBy)
import qualified Data.ByteString.Char8 as C (ByteString, pack, unpack, writeFile,
    readFile)
import Codec.Compression.Snappy (compress, decompress)
import Control.DeepSeq

-- |A database has some metadata and tables.
data DB = DB Meta [Table] deriving (Show, Eq)

instance NFData (DB) where
  rnf (DB _ tables) = map rnf tables `deepseq` ()

-- |Table's metadata is empty.
data Meta = Meta deriving (Show, Eq)

-- |A table has a name, a list of columns' types and rows.
data Table = Table String [Column] [Row] deriving (Read, Show, Eq)

instance NFData (Table) where
  rnf (Table _ cols rows) = map rnf cols `deepseq` map rnf rows `deepseq` ()

-- |A table's column which has a name and a type.
data Column = Column String Type deriving (Read, Show, Eq)

instance NFData (Column) where
  rnf (Column n t) = n `seq` t `seq` ()

-- |Row consists of a list of values.
data Row = Row [Value] deriving (Read, Show, Eq)

instance NFData (Row) where
  rnf (Row values) = map rnf values `deepseq` ()

-- |query is function responsible for executing Query tokens.
query :: DB -> [Query] -> Maybe (DB, [[Value]])
query db (CreateTable name rows : _) = Just $!! (createTable db name rows, [])
query db (DropTable name : _) = fmap (const (db, [])) $ dropTable db name

query db@(DB _ tables) (SelectAll tableName : stmts) =
  query db (Select tableName colNames : stmts)
  where (Table _ cols _) = head $ filter (\(Table n _ _) -> n == tableName) tables
        colNames = map (\(Column n _) -> n) cols

query db@(DB _ tables) (Select tableName selectedColumns : stmts) =
  Just (db, selRows)
  where selRows = map (\(Row values) -> values) newQRows
        newQRows = map colSelect rows
        colSelect (Row values) = Row $ map (values !!) colIds
        maybeColIds = map (`elemIndex` colNames) selectedColumns
        colNames = map (\(Column cName _)-> cName) columns
        colIds = map (\(Just int)->int) maybeColIds
        (Table _ columns rows) = fst $ foldl constrain (table, emptyTable) stmts
        emptyTable = Table "" [] []
        table = head $ filter (\(Table n _ _) -> n == tableName) tables

query db (Insert name values : stmts) =
  insertRow db name values >>= \db' -> Just $!! (db', [])

query db@(DB _ tables) (Delete tableName : stmts) = Just (modifiedDB, [])
  where Just modifiedDB = modifyTable db tableName deleteRows
        deleteRows _ = Table tableName cols rej
        table = head $ filter (\(Table n _ _) -> n == tableName) tables
        emptyTable = Table "" [] []
        (Table _ cols _, (Table _ _ rej)) = foldl constrain (table, emptyTable) stmts
-- |Update query set new values in selected columns of the qRows list.
-- |After update concatenation of qRows and notQRows is placed as new
-- |table rows set.
query db@(DB _ tables) (Update name newValues : stmts) = Just (modifiedDB, [])
    where
        (Table _ columns rows) = fst $ foldl constrain (table, emptyTable) stmts
        emptyTable = Table "" [] []
        table = head $ filter (\(Table n _ _) -> n == name) tables
        (Table _ cols acc, (Table _ _ rej)) = foldl constrain (table, emptyTable) stmts
        Just modifiedDB = modifyTable db name (modValues columns newValues acc rej)
        modValues ::
            [Column] -> [(String, Value)] -> [Row] -> [Row] -> Table -> Table
        modValues columns newValues acc rej table = newTable table
        newTable (Table name cols _) = Table name cols newRows
        newRows = updatedRows ++ rej
        updatedRows = map rowUpdate acc
        indexValues = map extract newValues
        columnNames = map (\(Column name _)->name) columns
        extract (string, value) = case (elemIndex string columnNames) of
            Nothing  -> error $ "No such column: " ++ string
            Just int -> (int, value)
        rowUpdate (Row values) = Row $ correct values indexValues
        correct :: [Value] -> [(Int, Value)] -> [Value]
        correct values [] = values
        correct values ((index, value):indexValues) = correct
            (checkAndCorrectRow values index value (columns !! index))
            indexValues
        checkAndCorrectRow :: [Value] -> Int -> Value -> Column -> [Value]
        checkAndCorrectRow values index
            (StringValue str) (Column _ String) =
                correctRow values index (StringValue str)
        checkAndCorrectRow values index (IntValue int) (Column _ Int) =
            correctRow values index (IntValue int)
        checkAndCorrectRow _ _ _ _ = error "Incorrect types!"
        correctRow values index value =
            ((take index values)
            ++ [value]
            ++ (snd $ splitAt (index + 1) values))

constrain :: (Table, Table) -> Query -> (Table, Table)
constrain ((Table name cols qRows), (Table _ _ notQRows)) (Where conds) =
  ((Table name cols acc), (Table name cols rej))
  where
        (acc, rej) =
            foldr rowWalker ([], notQRows) qRows where
              rowWalker (row@(Row values)) (qRows, notQRows) =
                   if whereWalker conds values
                     then (row:qRows, notQRows)
                     else (qRows, row:notQRows)
              whereWalker :: WhereConditions -> [Value] -> Bool
              whereWalker (OrConditions conditions) values =
                   or (map (`whereWalker` values) conditions)
              whereWalker (AndConditions conditions) values =
                   and (map (`whereWalker` values) conditions)
              whereWalker (Condition colName comparer) values =
                   comparer $ getCell colName values
              getCell colName values = values !! getColId colName
              getColId colName =
                   case elemIndex colName (map (\(Column name _)->name) cols) of
                       Nothing  -> error $ "No such column: " ++ colName
                       Just int -> int

-- |OrderBy query sorts a qRows list, comparing values from
-- |provided columns with selected order. The list of columns used to sort
-- |a qRows list is reversed. Each column and its order is used in comparing
-- |function used in stable sorting algorithm provided by 'sortBy'.
constrain (Table name cols qRows, rej) (OrderBy orderBy) =
  (Table name cols sortedQRows, rej)
    where sortedQRows = colSort qRows $ reverse orderBy
          colSort qrows [] = qrows
          colSort rows ((cName, ord):orderBy) =
              colSort (sortBy (cmp cName ord) rows) orderBy
          cmp colName Ascending (Row valuesTwo) (Row valuesOne) =
              orderingValue (valuesTwo !! colIndex colName)
                  (valuesOne !! colIndex colName)
          cmp colName Descending (Row valuesTwo) (Row valuesOne) =
              orderingValue (valuesOne !! colIndex colName)
                  (valuesTwo !! colIndex colName)
          orderingValue :: Value -> Value -> Ordering
          orderingValue valOne valTwo | valOne == valTwo  = EQ
                                      | valOne >  valTwo  = GT
                                      | valOne <  valTwo  = LT
          colIndex name = case elemIndex name columnNames of
              Nothing  -> error $ "No such column: " ++ name
              Just int -> int
          columnNames = map (\(Column name _)->name) cols

constrain ((Table name cols acc), (Table _ _ rej)) (Limit num) =
  ((Table name cols acc'), (Table name cols (rej' ++ rej)))
  where (acc', rej') = splitAt num acc

-- |Creates a new DB instance.
new :: DB
new = DB Meta []

-- |Loads an existing DB from a serialised form.
load :: C.ByteString -> DB
load bytestring = DB Meta tables
  where tables = (read . C.unpack . decompress) bytestring

-- |Adds a table to a given database.
createTable :: DB
            -> String -- The name of the added table
            -> [(String, Type)] -- Names and types of values stored in the table
            -> DB
createTable _ _ [] = error "Tables without columns are illegal."
createTable _ "" _ = error "Table's name is mandatory."
createTable db@(DB meta tables) name cols =
  case findTable db name of
    Just _  -> error $ "Table: '" ++ name ++ "' already exists."
    Nothing -> DB meta newTables
  where newTables  = addedTable : tables
        addedTable = Table name (map (uncurry Column) cols) []

-- |Drops a table from a given database.
dropTable :: DB
          -> String -- The name of the dropped table
          -> Maybe DB
dropTable _ "" = error "Table's name is mandatory."
dropTable db@(DB meta tables) name =
    findTable db name >> Just (DB meta (drop tables))
    where drop = filter (\(Table thisName _ _) -> name /= thisName)

-- |Used to apply a given function to the table with a given name.
modifyTable :: DB
            -> String           -- The name of a table to modify
            -> (Table -> Table) -- The modifying function
            -> Maybe DB
modifyTable db@(DB meta tables) name fun =
  findTable db name >> Just (DB meta (modTable tables))
  where modTable [] = []
        modTable (t@(Table thisName _ _):ts)
          | thisName == name = fun t : ts
          | otherwise        = t : modTable ts

---- |Dumps the database on the HDD.
dump :: DB -> C.ByteString
dump (DB _ tables) = compress $ C.pack $ show tables

---- |Inserts a new row to a given table. It should check all types and constraints.
insertRow :: DB -> String -> [Value] -> Maybe DB
insertRow db name values = modifyTable db name addRow
    where addRow :: Table -> Table
          addRow (Table _ columns rows) = Table name columns (rows ++ [newRow])
              where newRow = Row $ buildNewRow (types columns) values
                    types = map (\(Column _ t) -> t)
                    buildNewRow [] [] = []
                    buildNewRow (String:restTs) (val@(StringValue _):restVs) =
                        val:buildNewRow restTs restVs
                    buildNewRow (Int:restTs) (val@(IntValue _):restVs) =
                        val:buildNewRow restTs restVs
                    buildNewRow _ _ = error "Incorrect types!"

-- |Returns a table with a given name. Returns Nothing if there's no table with
-- such name.
findTable :: DB -> String -> Maybe Table
findTable (DB _ tables) name = findByName tables
    where findByName :: [Table] -> Maybe Table
          findByName [] = Nothing
          findByName (table@(Table thisName _ _):ts)
            | thisName == name = Just table
            | otherwise        = findByName ts

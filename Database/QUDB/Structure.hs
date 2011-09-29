module Database.QUDB.Structure (new, load, dump, query, DB) where

import Database.QUDB.EntityTypes
import Database.QUDB.Query
import Database.QUDB.Utils (sortByM)
import Data.List (elemIndex)
import Data.Foldable (foldlM, foldrM)
import qualified Data.ByteString.Char8 as C (ByteString, pack, unpack)
import qualified Control.Monad.Error as E (Error, throwError)
import Codec.Compression.Snappy (compress, decompress)

-- |A database has some metadata and tables.
data DB = DB Meta [Table] deriving (Show, Read, Eq)

-- |Database's metadata consists of the format's version identifier.
data Meta = Meta Version deriving (Show, Read, Eq)

-- |Version identifier is a (major, minor) pair.
data Version = Version Int Int deriving (Show, Read, Eq)

-- |A table has a name, a list of columns' types and rows.
data Table = Table String [Column] [Row] deriving (Read, Show, Eq)

-- |A table's column which has a name and a type.
data Column = Column String Type deriving (Read, Show, Eq)

-- |Row consists of a list of values.
data Row = Row [Value] deriving (Read, Show, Eq)

-- |A monad in which queries are executed.
type DBMonad = Either Error

data Error = NoSuchColumn String
           | NoSuchTable String
           | TableMustHaveColumns
           | NoTableNameGiven
           | TableExists String
           | TypeMismatch
           deriving Show

instance E.Error Error

-- |query is function responsible for executing Query tokens.
query :: DB -> [Query] -> DBMonad (DB, [[Value]])

query db@(DB meta tables) (CreateTable name rows : _) =
  createTable name rows >>= \x -> return (x, [])
  where createTable _ [] = E.throwError TableMustHaveColumns
        createTable "" _ = E.throwError NoTableNameGiven
        createTable _ _ =
          case findTable db name of
            Right _  -> E.throwError $ TableExists name
            Left _ -> return (DB meta newTables)
        newTables  = addedTable : tables
        addedTable = Table name (map (uncurry Column) rows) []

query db (DropTable name : _) = dropTable name >> return (db, [])
  where
  DB meta tables = db
  dropTable "" = E.throwError NoTableNameGiven
  dropTable name = findTable db name >> return (DB meta (drop tables))
  drop = filter (\(Table thisName _ _) -> name /= thisName)

query db@(DB _ tables) (SelectAll tableName : stmts) =
  query db (Select tableName colNames : stmts)
  where (Table _ cols _) = head $ filter (\(Table n _ _) -> n == tableName) tables
        colNames = map (\(Column n _) -> n) cols

query db@(DB _ tables) (Select tableName selectedColumns : stmts) =
  do (Table _ columns rows, _) <- constrain table stmts
     values <- mapM (colSelect columns) rows
     return (db, values)
  where colSelect cols (Row values) = do ids <- columnIDs cols
                                         return $ map (values !!) ids
        columnIDs cols = mapM (colIDByName cols) selectedColumns
        colIDByName :: [Column] -> String -> DBMonad Int
        colIDByName cols name = case name `elemIndex` colNames cols of
                                  Nothing -> E.throwError $ NoSuchColumn name
                                  Just id -> return id
        colNames = map $ \(Column name _) -> name
        table = head $ filter (\(Table n _ _) -> n == tableName) tables

query db (Insert name values : _) = do db' <- insertRow db name values
                                       return (db', [])

query db@(DB _ tables) (Delete tableName : stmts) =
  do (Table _ cols _, Table _ _ rej) <- constrain table stmts
     db' <- modifyTable db tableName (deleteRows cols rej)
     return (db', [])
  where deleteRows cols rej _ = return $ Table tableName cols rej
        table = head $ filter (\(Table n _ _) -> n == tableName) tables

-- |Update query set new values in selected columns of the qRows list.
-- |After update concatenation of qRows and notQRows is placed as new
-- |table rows set.
query db@(DB _ tables) (Update name newValues : stmts) =
  do (Table _ cols acc, Table _ _ rej) <- constrain table stmts
     db' <- modifyTable db name $ newTable cols acc rej
     return (db', [])
  where table = head $ filter (\(Table n _ _) -> n == name) tables
        newTable cols acc rej _ = do newRows <- fmap (++rej) (updatedRows acc cols)
                                     return $ Table name cols newRows
        updatedRows acc cols = mapM (rowUpdate cols) acc
        indexedValues cols = mapM (colNamesToIndices cols) newValues
        columnNames = map (\(Column name _) -> name)
        colNamesToIndices :: [Column] -> (String, Value) -> DBMonad (Int, Value)
        colNamesToIndices cols (name, value) =
          case elemIndex name (columnNames cols) of
            Nothing    -> E.throwError $ NoSuchColumn name
            Just index -> return (index, value)
        rowUpdate cols (Row values) = do ivs <- indexedValues cols
                                         updated <- correct cols values ivs
                                         return $ Row updated
        correct :: [Column] -> [Value] -> [(Int, Value)] -> DBMonad [Value]
        correct cols values [] = return values
        correct cols values ((index, val):rest) =
          do updated <- checkAndCorrectRow values index val (cols !! index)
             correct cols updated rest
        checkAndCorrectRow :: [Value] -> Int -> Value -> Column -> DBMonad [Value]
        checkAndCorrectRow values index (StringValue str) (Column _ String) =
          return $ correctRow values index (StringValue str)
        checkAndCorrectRow values index (IntValue int) (Column _ Int) =
          return $ correctRow values index (IntValue int)
        checkAndCorrectRow _ _ _ _ = E.throwError TypeMismatch
        correctRow values index value =
            ((take index values)
            ++ [value]
            ++ (snd $ splitAt (index + 1) values))

constrain :: Table -> [Query] -> DBMonad (Table, Table)
constrain table stmts = foldlM constrainStep (table, emptyTable) stmts
  where emptyTable = Table "" [] []

constrainStep :: (Table, Table) -> Query -> DBMonad (Table, Table)

constrainStep ((Table name cols qRows), (Table _ _ notQRows)) (Where conds) =
  do (acc, rej) <- foldrM rowWalker ([], notQRows) qRows
     return (Table name cols acc, Table name cols rej)
  where rowWalker :: Row -> ([Row], [Row]) -> DBMonad ([Row], [Row])
        rowWalker (row@(Row values)) (accepted, rejected) =
          do matches <- whereWalker conds values
             return $ if matches
                           then (row:accepted, rejected)
                           else (accepted, row:rejected)
        whereWalker :: WhereConditions -> [Value] -> DBMonad Bool
        whereWalker (OrConditions conditions) values =
             fmap or (mapM (`whereWalker` values) conditions)
        whereWalker (AndConditions conditions) values =
             fmap and (mapM (`whereWalker` values) conditions)
        whereWalker (Condition colName comparer) values =
          do cell <- getCell colName values
             return $ comparer cell
        getCell name values = fmap (values !!) $ getColId name
        getColId name = case name `elemIndex` (columnNames cols) of
                          Nothing  -> E.throwError $ NoSuchColumn name
                          Just int -> return int
        columnNames = map (\(Column name _)->name)

-- |OrderBy query sorts a qRows list, comparing values from
-- |provided columns with selected order. The list of columns used to sort
-- |a qRows list is reversed. Each column and its order is used in comparing
-- |function used in stable sorting algorithm provided by 'sortByM'.
constrainStep (Table name cols qRows, rej) (OrderBy orderBy) =
  do sortedRows <- colSort qRows $ reverse orderBy
     return (Table name cols sortedRows, rej)
    where colSort rows [] = return rows
          colSort rows ((colName, ord):orderBy) =
            do sorted <- sortByM (cmp colName ord) rows
               colSort sorted orderBy
          cmp colName Ascending (Row valuesTwo) (Row valuesOne) =
            do index <- colIndex colName
               return $ orderingValue (valuesTwo !! index) (valuesOne !! index)
          cmp colName Descending x y = cmp colName Ascending y x
          orderingValue :: Value -> Value -> Ordering
          orderingValue valOne valTwo | valOne == valTwo  = EQ
                                      | valOne >  valTwo  = GT
                                      | valOne <  valTwo  = LT
          colIndex :: String -> DBMonad Int
          colIndex name = case name `elemIndex` columnNames of
              Nothing  -> E.throwError $ NoSuchColumn name
              Just int -> return int
          columnNames = map (\(Column name _)->name) cols

constrainStep ((Table name cols acc), (Table _ _ rej)) (Limit num) =
  return ((Table name cols acc'), (Table name cols (rej' ++ rej)))
  where (acc', rej') = splitAt num acc

-- |The current DB format's version identifier.
version :: Version
version = Version 0 0

-- |Creates a new DB instance.
new :: DB
new = DB (Meta version) []

-- |Loads an existing DB from a serialised form.
load :: C.ByteString -> DB
load = read . C.unpack . decompress

-- |Used to apply a given function to the table with a given name.
modifyTable :: DB
            -> String                   -- The name of a table to modify
            -> (Table -> DBMonad Table) -- The modifying function
            -> DBMonad DB
modifyTable db@(DB meta tables) name fun =
  findTable db name >> modTable tables >>= \tables' -> return (DB meta tables')
  where modTable :: [Table] -> DBMonad [Table]
        modTable [] = return []
        modTable (t@(Table thisName _ _):ts)
          | thisName == name = do t' <- fun t
                                  return $ t' : ts
          | otherwise        = fmap (t:) $ modTable ts

-- |Dumps the database to a bytestring, which can be later loaded using the
-- load function.
dump :: DB -> C.ByteString
dump = compress . C.pack . show

---- |Inserts a new row to a given table. It should check all types and constraints.
insertRow :: DB -> String -> [Value] -> DBMonad DB
insertRow db name values = modifyTable db name addRow
    where addRow :: Table -> DBMonad Table
          addRow (Table _ columns rows) = do new <- newRow
                                             return $ Table name columns (rows ++ [new])
              where newRow = fmap (Row) $ buildNewRow (types columns) values
                    types = map (\(Column _ t) -> t)
                    buildNewRow [] [] = return []
                    buildNewRow (String:restTs) (val@(StringValue _):restVs) =
                      do rest <- buildNewRow restTs restVs
                         return $ val:rest
                    buildNewRow (Int:restTs) (val@(IntValue _):restVs) =
                      do rest <- buildNewRow restTs restVs
                         return $ val:rest
                    buildNewRow _ _ = E.throwError TypeMismatch

-- |Returns a table with a given name. Throws an error if there's no table with
-- such name.
findTable :: DB -> String -> DBMonad Table
findTable (DB _ tables) name = findByName tables
    where findByName :: [Table] -> DBMonad Table
          findByName [] = E.throwError $ NoSuchTable name
          findByName (table@(Table thisName _ _):ts)
            | thisName == name = return table
            | otherwise        = findByName ts

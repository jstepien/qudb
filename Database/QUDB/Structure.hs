module Database.QUDB.Structure (initDB, loadDB, dumpDB, query, DB) where

import Database.QUDB.EntityTypes
import Database.QUDB.Query
import Data.IORef
import Data.List (elemIndex, sortBy)
import Data.Time.Clock
import qualified Data.ByteString.Lazy.Char8 as C (pack, unpack, writeFile,
    readFile)
import Codec.Compression.Zlib (compress, decompress)
import Control.Concurrent (forkIO)

-- |A database has some metadata and a IO reference to tables.
data DB = DB Meta (IORef [Table])

-- |Table's metadata consists of it's file name and a timestamp of the last
-- dump.
data Meta = Meta String (IORef UTCTime)

-- |A table has a name, a list of columns' types and rows.
data Table = Table String [Column] [Row] deriving (Read, Show)

-- |A table's column which has a name and a type.
data Column = Column String Type deriving (Read, Show)

-- |Row consists of a list of values.
data Row = Row [Value] deriving (Read, Show)

-- |QTable is representation of db Table with an extra data.
-- |Is used when processing query.
-- |Table is original table.
-- |First list of Rows represent selected, and second one those unselected.
data QTable = QTable Table [Row] [Row] | EmptyQTable 

-- |The interval between database dumps on HDD.
dumpInterval :: NominalDiffTime
dumpInterval = 60

-- |query is function responsible for executing Query tokens.
query :: DB -> [Query] -> IO [[Value]]
query db queries = do
    execQueries db $ order queries
    where
        execQueries db queries = do
            extract $ foldr (exeq db) initIOQTable queries
        initIOQTable :: IO (QTable)
        initIOQTable = do return (EmptyQTable)
        extract qTable = do
            qtable <- qTable
            case qtable of
                EmptyQTable -> return ( [[]] )
                (QTable  _ rows _ ) -> return
                    (map (\(Row values)->values) rows)
        -- From queries have to be on the end of the Query list
        order queries = filter (not . isFrom) queries ++ filter isFrom queries
                        where isFrom (From _) = True
                              isFrom _        = False

-- |Creates a new DB instance with a given file name.
initDB :: String -> IO DB
initDB filename = do
    tables <- newIORef []
    now <- getCurrentTime
    timestampIORef <- newIORef now
    return $ DB (Meta filename timestampIORef) tables

-- |Loads an existing DB from a given file.
loadDB :: String -> IO DB
loadDB filename = do
  bytestring <- C.readFile filename
  tables <- (newIORef . read . C.unpack . decompress) bytestring
  now <- getCurrentTime
  timestampIORef <- newIORef now
  return $ DB (Meta filename timestampIORef) tables

-- |Adds a table to a given database.
createTable :: DB
            -> String -- The name of the added table
            -> [(String, Type)] -- Names and types of values stored in the table
            -> IO ()
createTable _ _ [] = error "Tables without columns are illegal."
createTable _ "" _ = error "Table's name is mandatory."
createTable db@(DB _ tables) name cols = do
    existingTable <- findTable db name
    case existingTable of
        Just _  -> error $ "Table: '" ++ name ++ "' already exists."
        Nothing -> modifyIORef tables addTable
    where addTable oldTables = newTable : oldTables
          newTable = Table name (map (uncurry Column) cols) []

-- |Drops a table from a given database.
dropTable :: DB
          -> String -- The name of the dropped table
          -> IO ()
dropTable _ "" = error "Table's name is mandatory."
dropTable db@(DB _ tables) name = do
    existingTable <- findTable db name
    case existingTable of
        Just _  -> modifyIORef tables drop
        Nothing -> error $ "Table: '" ++ name ++ "' doesn't exists."
    where drop = filter (\(Table thisName _ _) -> name /= thisName)

-- |Used to apply a given function to the table with a given name.
modifyTable :: DB
            -> String           -- The name of a table to modify
            -> (Table -> Table) -- The modifying function
            -> IO ()
modifyTable db@(DB meta tablesRef) name fun = do
    table <- findTable db name
    case table of
        Nothing -> error $ "No such table: '" ++ name ++ "'."
        Just _  -> modifyIORef tablesRef modTable >> dumpIfNecessary meta
    where modTable [] = []
          modTable (t@(Table thisName _ _):ts)
            | thisName == name = fun t : ts
            | otherwise        = t : modTable ts
          dumpIfNecessary (Meta _ timeRef) = do
            dumpTime <- readIORef timeRef
            now <- getCurrentTime
            let diff = diffUTCTime now dumpTime
            if diff > dumpInterval
              then (forkIO $ dumpDB db) >> return ()
              else return ()

-- |Dumps the database on the HDD.
dumpDB :: DB -> IO ()
dumpDB (DB (Meta name dumpTimeRef) tablesRef) = do
    tables <- readIORef tablesRef
    now <- getCurrentTime
    modifyIORef dumpTimeRef (\_ -> now)
    C.writeFile name $ compress $ C.pack $ show tables

-- |Inserts a new row to a given table. It should check all types and constraints.
insertRow :: DB -> String -> [Value] -> IO ()
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
findTable :: DB -> String -> IO (Maybe Table)
findTable (DB _ tablesRef) name = fmap findByName $ readIORef tablesRef
    where findByName :: [Table] -> Maybe Table
          findByName [] = Nothing
          findByName (table@(Table thisName _ _):ts)
            | thisName == name = Just table
            | otherwise        = findByName ts

-- |Returns all values from given columns of a table with a given name.
getValues :: DB -> String -> [String] -> IO [[Value]]
getValues db name selectedCols = do
    table <- findTable db name
    case table of
        Just (Table _ cols rows) -> return $ map (valuesFilter cols) rows
        Nothing -> error $ "No such table: '" ++ name ++ "'."
    where valuesFilter :: [Column] -> Row -> [Value]
          valuesFilter tableCols (Row values) =
              map (values !!) columnIDs
              where columnIDs = map columnID selectedCols
                    columnID c = case elemIndex c (names tableCols) of
                                     Just int -> int
                                     Nothing -> error $ "No such column: '" ++ c
                                         ++ "'."
                    names = map (\(Column name _) -> name)

-- |Returns all values from a table with a given name.
getAllValues :: DB -> String -> IO [[Value]]
getAllValues db name = do
    table <- findTable db name
    case table of
        Just (Table _ _ rows) -> return $ buildValuesList rows
        Nothing -> error $ "No such table: '" ++ name ++ "'."
    where buildValuesList [] = []
          buildValuesList (Row values:rs) = values : buildValuesList rs

-------------------------------------------------------------------------------
--                  Definition of Qeuery execution.                          --
-------------------------------------------------------------------------------

-- |Method executing Query.
exeq :: DB -> Query -> IO (QTable) -> IO (QTable)

-- |From query is getting a Table with given name from db.
-- |If a table exists is wrapped with QTable.  
exeq db (From tableName) _ = do
    table <- findTable db tableName
    case table of
        Nothing -> error $ "No such table: "++tableName
        Just (tab@(Table _ _ rows)) -> return (QTable tab rows  [])

-- |Select all (synonym of *) returns unmodified QTable.
exeq _ (SelectAll) qtable = qtable

-- |Select query modifies qRows list removing from it unselected columns.
exeq db (Select selectedColumns) qtable = do
    (QTable (table@(Table tName tColumns tRows )) qRows notQRows) <- qtable
    return (selecteQTable selectedColumns table tColumns qRows notQRows)
    where
        selecteQTable selectedColumns table columns qRows notQRows =
            QTable table newQRows notQRows where
                newQRows = map colSelect qRows
                colSelect (Row values) = Row $ map (values !!) colIds
                maybeColIds = map (`elemIndex` colNames) selectedColumns
                colNames = map (\(Column cName _)-> cName) columns 
                colIds = map (\(Just int)->int) maybeColIds

-- |Update query set new values in selected columns of the qRows list. 
-- |After update concatenation of qRows and notQRows is placed as new 
-- |table rows set.
exeq db (Update newValues) qtable = do
    (QTable (table@(Table name columns _)) qRows notQRows) <- qtable
    modifyTable db name (modValues columns newValues qRows notQRows)
    newTable <- findTable db name 
    case newTable of
        Nothing  -> error $ "No such table: "++ name
        Just _   -> return EmptyQTable 
    where
        modValues :: 
            [Column] -> [(String, Value)] -> [Row] -> [Row] -> Table -> Table
        modValues columns newValues qRows notQRows table = newTable table where
            newTable (Table name cols _) = Table name cols newRows
            newRows = updatedRows ++ notQRows
            updatedRows = map rowUpdate qRows
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
                

-- |OrderBy query sorts a qRows list, comparing values from
-- |provided columns with selected order. The list of columns used to sort
-- |a qRows list is reversed. Each column and its order is used in comparing
-- |function used in stable sorting algorithm provided by 'sortBy'.
exeq db (OrderBy orderBy) qtable = do
    (QTable (table@(Table name columns _)) qRows notQRows) <- qtable
    return (QTable table (sortedQRows qRows orderBy columns) notQRows)
    where
        sortedQRows qRows orderBy columns =
            colSort qRows $ reverse orderBy where
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
                columnNames = map (\(Column name _)->name) columns

-- |Insert query add new row to a existing table.
exeq db (Insert values) qtable = do
    (QTable (table@(Table name _ _)) qRows notQRows) <- qtable
    insertRow db name values
    return EmptyQTable 

-- |Delete query sets rows in a provided table to notQRows.
exeq db Delete qtable = do 
    (QTable (table@(Table name _ _)) qRows notQRows) <- qtable
    modifyTable db name (deleteRows notQRows)
    return EmptyQTable
    where
        deleteRows notQRows (Table name columns rows) = 
            (Table name columns notQRows)

-- |This query take given number of rows from top of qRows, and replace it.
-- |notQRows are extended by unselected rows from a qRows list.
exeq _ (Top top) qtable = do
    (QTable table qRows notQRows) <- qtable
    return $ newQTable table qRows notQRows top where
        newQTable table qrows notqrows top = 
            QTable table newQRows newNotQRows where
                newQRows = take top qrows
                newNotQRows = (snd $ splitAt top newQRows)++notqrows

-- |Where query is checking each row from qRows with set,
-- |with 'where' conditions. Rows not passing conditions are removed
-- |from qRows and added to notQRows.
exeq db (Where whereConditions) qtable = do 
    (QTable (table@(Table tName tCols tRows)) qRows notQRows) <- qtable
    return $ uncurry (QTable table) $ newRows table tCols qRows notQRows 
    where
        newRows table tCols qRows notQRows = 
            foldr rowWalker ([], notQRows) qRows where
              rowWalker (row@(Row values)) (qRows, notQRows) =
                   case whereWalker whereConditions values of
                        True  -> (row:qRows, notQRows)
                        False -> (qRows, row:notQRows)
              whereWalker :: WhereConditions -> [Value] -> Bool
              whereWalker (OrConditions conditions) values = 
                   foldr1 (||) (map (`whereWalker` values) conditions)
              whereWalker (AndConditions conditions) values = 
                   foldr1 (&&) (map (`whereWalker` values) conditions)
              whereWalker (Condition colName comparer) values = 
                   comparer $ getCell colName values
              getCell colName values = values !! getColId colName
              getColId colName =
                   case elemIndex colName (map (\(Column name _)->name) tCols) of
                       Nothing  -> error $ "No such column: " ++ colName 
                       Just int -> int 

-- |Create query creates a new table.
exeq db (CreateTable name columns) _ = do
    createTable db name columns 
    return EmptyQTable

-- |Drop query remove the table.
exeq db (DropTable tableName) _ = do
    dropTable db tableName 
    return EmptyQTable

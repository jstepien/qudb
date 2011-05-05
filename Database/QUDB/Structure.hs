module Database.QUDB.Structure (
    initDB, createTable, insertRow, getValues, getAllValues, DB
    ) where

import Database.QUDB.EntityTypes
import Data.IORef
import Data.List (elemIndex)

-- |A database has a filename and a IO reference to tables.
data DB = DB String (IORef [Table])

-- |A table has a name, a list of columns' types and rows.
data Table = Table String [Column] [Row]

-- |A table's column which has a name and a type.
data Column = Column String Type

-- |Row consists of a list of values.
data Row = Row [Value]

-- |Creates a new DB instance with a given filename.
initDB :: String -> IO DB
initDB filename = do
    tables <- newIORef []
    return $ DB filename tables

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

-- |Used to apply a given function to the table with a given name.
modifyTable :: DB
            -> String           -- The name of a table to modify
            -> (Table -> Table) -- The modifying function
            -> IO ()
modifyTable (DB _ tablesRef) name fun = modifyIORef tablesRef modTable
    where modTable [] = []
          modTable (t@(Table thisName _ _):ts)
            | thisName == name = fun t : ts
            | otherwise        = t : modTable ts

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

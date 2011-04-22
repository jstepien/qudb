module Database.QUDB.Structure (
    initDB, createTable, insertRow, getTable, getValues, DB
    ) where

import Database.QUDB.EntityTypes
import Data.IORef

-- |A database has a filename and a IO reference to tables.
data DB = DB String (IORef [Table])

-- |A table has a name, a list of columns' types and rows.
data Table = Table String [Type] [Row]

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
            -> [Type] -- Types of values stored in the table
            -> IO ()
createTable _ _ [] = error "Tables without columns are illegal."
createTable _ "" _ = error "Table's name is mandatory."
createTable (DB _ tables) name colTypes = modifyIORef tables addTable
    where addTable oldTables = newTable : oldTables
          newTable = Table name colTypes []

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
          addRow (Table _ types rows) = Table name types (rows ++ [newRow])
              where newRow = Row $ buildNewRow types values
                    buildNewRow [] [] = []
                    buildNewRow (String:restTs) (val@(StringValue _):restVs) =
                        val:buildNewRow restTs restVs
                    buildNewRow (Int:restTs) (val@(IntValue _):restVs) =
                        val:buildNewRow restTs restVs
                    buildNewRow _ _ = error "Incorrect types!"

-- |Finds a table by name.
getTable :: DB -> String -> IO (Maybe Table)
getTable (DB _ tablesRef) name = readIORef tablesRef >>= return . find
    where find [] = Nothing
          find (t@(Table thisName _ _):ts)
            | thisName == name = Just t
            | otherwise        = find ts

-- |Returns all values from a table.
getValues :: Table -> [[Value]]
getValues (Table _ _ rows) = buildValuesList rows
    where buildValuesList [] = []
          buildValuesList (Row values:rs) = values : buildValuesList rs

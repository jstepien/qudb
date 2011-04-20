module Database.QUDB.Query where

import Database.QUDB.EntityTypes
import Database.QUDB.Structure

-- |Queries which can be sent to the database.
data Query = Select String         -- ^Accepts the name of a table.
           | Insert String [Value] -- ^Accepts the name of a table and a list
                                   -- of values to insert.
           deriving Show

-- |Executes a query. Select queries return a list of rows containing values.
-- Other queries return an empty collection.
query :: DB -> Query -> IO [[Value]]
query db (Insert name values) = modifyTable db name insertRow >> return []
    where insertRow (Table _ types rows) = Table name types (rows ++ [newRow])
            where newRow = Row $ buildNewRow types values
                  buildNewRow [] [] = []
                  buildNewRow (String:restTs) (val@(StringValue _):restVs)
                    = val:buildNewRow restTs restVs
                  buildNewRow (Int:restTs) (val@(IntValue _):restVs)
                    = val:buildNewRow restTs restVs
                  buildNewRow _ _ = error "Incorrect types!"
query db (Select name) = do
    maybeTable <- getTable db name
    case maybeTable of
        Nothing    -> return []
        Just table -> return $ getValues table

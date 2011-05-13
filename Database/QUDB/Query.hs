module Database.QUDB.Query (
    Query(Select, Insert, CreateTable, DropTable), query
    ) where

import Database.QUDB.EntityTypes
import Database.QUDB.Structure

-- |Queries which can be sent to the database.
data Query = Select String [String] -- ^Accepts the name of a table and columns
           | Insert String [Value]  -- ^Accepts the name of a table and a list
                                    -- of values to insert.
           | CreateTable String [(String, Type)]
           | DropTable String       -- ^Drops a table with a given name
           deriving Show

-- |Executes a query. Select queries return a list of rows containing values.
-- Other queries return an empty collection.
query :: DB -> Query -> IO [[Value]]
query db (Insert name values) = noResult $ insertRow db name values
query db (Select name []) = getAllValues db name
query db (Select name columns) = getValues db name columns
query db (CreateTable name colums) = noResult $ createTable db name colums
query db (DropTable name) = noResult $ dropTable db name

noResult :: IO a -> IO [[Value]]
noResult x = x >> return []

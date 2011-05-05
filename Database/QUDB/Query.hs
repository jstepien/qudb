module Database.QUDB.Query (
    Query(Select, Insert, CreateTable), query
    ) where

import Database.QUDB.EntityTypes
import Database.QUDB.Structure

-- |Queries which can be sent to the database.
data Query = Select String         -- ^Accepts the name of a table.
           | Insert String [Value] -- ^Accepts the name of a table and a list
                                   -- of values to insert.
           | CreateTable String [(String, Type)]
           deriving Show

-- |Executes a query. Select queries return a list of rows containing values.
-- Other queries return an empty collection.
query :: DB -> Query -> IO [[Value]]
query db (Insert name values) = noResult $ insertRow db name values
query db (Select name) = getValues db name
query db (CreateTable name colums) = noResult $ createTable db name colums

noResult :: IO a -> IO [[Value]]
noResult x = x >> return []

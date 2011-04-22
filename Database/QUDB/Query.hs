module Database.QUDB.Query (
    Query(Select, Insert), query
    ) where

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
query db (Insert name values) = insertRow db name values >> return []
query db (Select name) = getValues db name

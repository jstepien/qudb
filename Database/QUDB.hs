module Database.QUDB (
    initDB, createTable,
    Type(Int, String), Value(IntValue, StringValue),
    query,
    parse
    ) where

import Database.QUDB.Structure
import Database.QUDB.Query
import Database.QUDB.EntityTypes
import Database.QUDB.Parser

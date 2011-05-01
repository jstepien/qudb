module Database.QUDB (
    initDB, query
    ) where

import Database.QUDB.Structure
import qualified Database.QUDB.Query as Q
import Database.QUDB.EntityTypes
import Database.QUDB.Parser (parse)

query :: DB -> String -> IO [[Value]]
query db str = Q.query db $ parse str

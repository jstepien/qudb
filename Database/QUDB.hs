module Database.QUDB (S.initDB, S.loadDB, S.dumpDB, query, S.DB) where

import qualified Database.QUDB.Structure as S
import Database.QUDB.EntityTypes (Value)
import Database.QUDB.Parser (parse)

query :: S.DB -> String -> IO [[Value]]
query db str = S.query db $ parse str

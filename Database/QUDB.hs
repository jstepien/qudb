module Database.QUDB (S.initDB, S.loadDB, S.dumpDB, query, S.DB,
    Value(IntValue, StringValue)) where

import qualified Database.QUDB.Structure as S
import Database.QUDB.EntityTypes (Value(IntValue, StringValue))
import Database.QUDB.Parser (parse)

query :: S.DB -> String -> Maybe (S.DB, [[Value]])
query db str = S.query db $ parse str

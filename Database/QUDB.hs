module Database.QUDB (S.new, S.load, S.dump, query, S.DB,
    Value(IntValue, StringValue)) where

import qualified Database.QUDB.Structure as S
import Database.QUDB.EntityTypes (Value(IntValue, StringValue))
import Database.QUDB.Parser (parse)

query :: S.DB -> String -> Either String (S.DB, [[Value]])
query db str = case parse str of
                 Left msg -> Left msg
                 Right q  -> case S.query db q of
                               Left error    -> Left $ show error
                               Right results -> Right results

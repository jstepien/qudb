module Database.QUDB (S.new, S.load, S.dump, query, S.DB,
    Value(IntValue, StringValue)) where

import qualified Database.QUDB.Structure as S
import Database.QUDB.EntityTypes (Value(IntValue, StringValue))
import Database.QUDB.Parser (parse)

query :: S.DB -> String -> Either String (S.DB, [[Value]])
query db str = case parse str of
                 Left error    -> Left error
                 Right queries -> case S.query db queries of
                                    Nothing  -> Left "Query failed."
                                    Just res -> Right res

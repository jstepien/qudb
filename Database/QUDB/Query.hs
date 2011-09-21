module Database.QUDB.Query where

import Database.QUDB.EntityTypes

-- |Defines order.
data Order = Descending | Ascending

-- |This data type is used in Where QOperation,
-- |to allow complex logic expressions.
data WhereConditions =
          Condition  String (Value->Bool)   -- Define column name,
                                            -- value comparer, taking value
                                            -- from each row of given column id
                                            -- and returning True or False.

        | OrConditions [WhereConditions]    -- Or logic sum
        | AndConditions [WhereConditions]   -- And logic sum

-- |Query part defined by QOpetation and QArg.
data Query = Select String [String]
           | SelectAll String
           | Update String [(String, Value)]
           | Insert String [Value]
           | Delete String
           | From String
           | Where WhereConditions
           | OrderBy [(String, Order)]
           | Limit Int
           | CreateTable String [(String, Type)]
           | DropTable String

module Database.QUDB.Query (
    Query(Select, Insert, CreateTable), query
    ) where

import Database.QUDB.EntityTypes
import Database.QUDB.Structure

query :: DB -> [ExecutableQuery] -> IO [[Value]]
query queries = foldr1 exeq queries


-- |Defines order.
data Order = Descending | Ascending

-- |This data type is used in Where QOperation to allow complex logic expressions.
data WhereConditions =
          Condition (Integer, (Value->Bool))-- Define column id,
                                            -- value comparer, taking value
                                            -- from each row of given column id
                                            -- and returning True or False.
                    
        | OrConditions [WhereConditions]    -- Or logic sum
        | AndConditions [WhereConditions]   -- And logic sum

-- |Available query operations as a part of query send to data base.
data QOperation = Select | Update | Insert | Delete | From | Where | OrderBy | Top

-- |Arguments of QOperation, each of them is related only to one operation.
data QArg = SelectArg [Integer]           -- column ids to return.
          | UpdateArg [(Integer, Value)]  -- column ids and a value assigned to them.
          | InsertArg [Row]               -- rows to insert.
          | DeleteArg    
          | FromArg Table               
          | WhereArg WhereConditions      -- read above.
          | OrderByArg [(Integer, Order)] -- column id and applied order.
          | TopArg Integer                -- number of top rows to return.

-- |Query part defined by QOpetation and QArg.
data Query = Query QOperation QArg

-- |QTable is representation of db Table with extra data. Is used when processing query.
-- |qTable is original table,
-- |qRows are selected rows and notQRows is list of other Table rows.
data QTable = QTable {qTable::Table, qRows, notQRows::[Row]}

class ExecutableQuery eq where
    exeq :: eq -> QTable -> QTable

instance ExecutableQuery Query where
    exeq eq qtable = qtable

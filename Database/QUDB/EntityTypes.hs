module Database.QUDB.EntityTypes (
  Type(String, Int),
  Value(IntValue, StringValue),
  ) where

import Control.DeepSeq

-- |Types of values which can be stored in columns.
data Type = Int | String deriving (Read, Show, Eq)

-- |Wrapped value of one of types from Type.
data Value = IntValue Int
           | StringValue String
           deriving (Show, Read, Ord, Eq)

instance NFData (Value)

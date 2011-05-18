module Database.QUDB.EntityTypes (
  Type(String, Int),
  Value(IntValue, StringValue),
  ) where

-- |Types of values which can be stored in columns.
data Type = Int | String deriving Show

-- |Wrapped value of one of types from Type.
data Value = IntValue Int
           | StringValue String
           deriving (Show, Ord, Eq)

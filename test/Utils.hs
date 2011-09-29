module Main where

import Database.QUDB.Utils
import Test.QuickCheck
import Data.List (sort)

main = test_sortByM

test_sortByM = do c integers
                  c reversed
                  c nothing
  where c :: (Testable t) => t -> IO ()
        c = quickCheck
        integers xs = sortByM cmp xs == Just (sort xs)
          where types = xs :: [Int]
        reversed xs = sortByM (flip $ cmp) xs == Just (reverse $ sort xs)
          where types = xs :: [Int]
        nothing xs = (not . null . drop 2) xs ==>
                     sortByM (const $ const Nothing) xs == Nothing
          where types = xs :: [Int]
        cmp x y = Just $ compare x y

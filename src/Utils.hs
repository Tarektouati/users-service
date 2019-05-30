module Utils (toInt, zero64) where

import Data.Int (Int64)

toInt :: String -> Int
toInt x = read x :: Int

zero64 :: Int64
zero64 = 0 :: Int64
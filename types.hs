{-# LANGUAGE RecordWildCards #-}
import Data.Map (Map, fromList)

data Customer = Customer { firstName :: String, lastName :: String, balance :: Int}
data Item = Item { name :: String, cost :: Int }
data Order = Order { buyer :: Customer, item :: Item }
data Thrider a b c = West a | North b | East c

extractThrider :: (Show a1, Show a2, Show a3) => Thrider a1 a2 a3 -> String
extractThrider (West a) = show a
extractThrider (North b) = show b
extractThrider (East b) = show b

(<=>) = map

test = ((\x -> x - 1) . (+ 1)) <=> [1..10]
module Fibs where

naiveFib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = naiveFib (n-1) + naiveFib (n-2)

naiveFibs :: [Integer]
naiveFibs = map naiveFib [0..]


fibs :: [Integer]
fibs = 0 : 1 : helper fibs (tail fibs)
    where
        helper (a:as) (b:bs) = a + b : helper as bs


module Exercise2 where

ownReverse :: [a] -> [a]
ownReverse [] = []
ownReverse (x:xs) = ownReverse xs <> [x]

reverseWithFoldl :: Foldable t => t a -> [a]
reverseWithFoldl = foldl (flip (:)) []

reverseWithFoldr :: Foldable t => t a -> [a]
reverseWithFoldr = foldr (\x y -> y <> [x]) []

zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zipWith'' f xs ys =
  [f (xs !! idx) (ys !! idx) | idx <- [0 .. len - 1]]
  where
    len = min (length xs) (length ys)

zipWithFoldl f as bs =
  reverse $ fst results
  where
    results = foldl applyFunction ([], as) bs
    applyFunction (zipped, []) _ = (zipped, [])
    applyFunction (zipped, x:xs) val = (f x val : zipped, xs)

concatMapFoldr f = foldl (\x y -> f x <> y) []

concatMapFoldl f = foldl (\x y -> y <> f x) []
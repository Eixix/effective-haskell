module Exercise2 where

ownReverse :: [a] -> [a]
ownReverse [] = []
ownReverse (x:xs) = ownReverse xs <> [x]

reverseWithFoldl :: Foldable t => t a -> [a]
reverseWithFoldl = foldl (flip (:)) []

reverseWithFoldr :: Foldable t => t a -> [a]
reverseWithFoldr = foldr (\x y -> y <> [x]) []

zipWith' f (x:xs) (y:ys) = f x y <> zipWith' f xs ys

zipWith'' f xs ys = [f x y | x <- xs, y <- ys]


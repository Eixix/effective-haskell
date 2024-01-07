{-# LANGUAGE DefaultSignatures #-}
module Main where

{-
data Natural a = Natural
    {
        equal :: a -> a -> Bool,
        add :: a -> a -> a,
        multiply :: a -> a -> a,
        additiveIdentity :: a,
        multiplicativeIdentity :: a,
        displayAsString :: a -> String
    }

intNatural :: Natural Int
intNatural = Natural
    {
        equal = (==),
        add = (+),
        multiply = (*),
        additiveIdentity = 0,
        multiplicativeIdentity = 1,
        displayAsString = show
    }
-}
data Peano = Z | S Peano

class (Show n, Eq n) => Natural n where
    add :: n -> n -> n
    multiply :: n -> n -> n
    additiveIdentity :: n
    multiplicativeIdentity :: n

instance Natural Int where
    add = (+)
    multiply = (*)
    additiveIdentity = 0
    multiplicativeIdentity = 1

instance Eq Peano where
    (==) Z Z = True
    (==) (S a) (S b) = a == b
    (==) _ _ = False

instance Show Peano where
    show Z = "Z"
    show (S a) = "(S " <> show a <> ")"

instance Natural Peano where
    add a Z = a
    add a (S b) = add (S a) b
    multiply Z _ = Z
    multiply (S a) b = add b (multiply a b)
    additiveIdentity = Z
    multiplicativeIdentity = S Z



class Redacted a where
    redacted :: a -> String
    default redacted :: Show a => a -> String
    redacted = show

data UserName = UserName String

instance Show UserName where
    show (UserName userName) = userName

instance Redacted UserName

data Password = Password String

instance Redacted Password where
    redacted _ = "<redacted>"

main :: IO ()
main = do
    putStrLn "Hallo"
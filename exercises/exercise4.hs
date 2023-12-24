module Exercise4 where

import Text.Read (readEither)

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

testStringTree :: BinaryTree String
testStringTree = Branch (Branch (Branch Leaf "2" Leaf) "1" (Branch Leaf "2" Leaf)) "0" (Branch (Branch Leaf "2" Leaf) "1" (Branch Leaf "2" Leaf))

testIntTree :: BinaryTree Int
testIntTree = Branch (Branch (Branch Leaf 2 Leaf) 1 (Branch Leaf 2 Leaf)) 0 (Branch (Branch Leaf 2 Leaf) 1 (Branch Leaf 2 Leaf))

-- Turn a binary tree of strings into a pretty-printed string
showStringTree :: BinaryTree String -> String
showStringTree tree =
  case tree of
    Branch t1 v t2 -> v <> "{" <> showStringTree t1 <> showStringTree t2 <> "}"
    _ -> ""

-- Add a new integer into a binary tree of integers
addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree tree n =
  case tree of
    Branch t1 v t2 -> Branch (addElementToIntTree t1 n) v t2
    Leaf -> Branch Leaf n Leaf

-- Check to see if an int value exists in a binary tree of ints
doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist tree n =
  case tree of
    Branch t1 v t2 -> (v == n) || (doesIntExist t1 n || doesIntExist t2 n)
    Leaf -> False

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval :: Expr -> Int
eval expr =
  case expr of
    Lit num -> num
    Add x y -> eval' (+) x y
    Sub x y -> eval' (-) x y
    Mul x y -> eval' (*) x y
    Div x y -> eval' div x y
  where
    eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
    eval' operator a b = operator (eval a) (eval b)

parse :: String -> Either String Expr
parse str =
  case parse' (words str) of
    Left err -> Left err
    Right (e, []) -> Right e
    Right (_, rest) -> Left $ "Found extra tokens: " <> unwords rest

parse' :: [String] -> Either String (Expr, [String])
parse' [] = Left "Unexpected end of expression"
parse' (token : rest) =
  case token of
    "+" -> parseBinary Add rest
    "*" -> parseBinary Mul rest
    "-" -> parseBinary Sub rest
    "/" -> parseBinary Div rest
    lit ->
      case readEither lit of
        Left err -> Left err
        Right lit' -> Right (Lit lit', rest)

parseBinary :: (Expr -> Expr -> a) -> [String] -> Either String (a, [String])
parseBinary exprConstructor args =
  case parse' args of
    Left err -> Left err
    Right (firstArg, rest') ->
      case parse' rest' of
        Left err -> Left err
        Right (secondArg, rest'') ->
          Right (exprConstructor firstArg secondArg, rest'')

run :: String -> String
run expr =
  case parse expr of
    Left err -> "Error: " <> err
    Right expr' ->
      let answer = show $ eval expr'
       in "The answer is: " <> answer

safeEval :: Expr -> Either String Int
safeEval expr =
  case expr of
    Lit num -> Right num
    Add x y -> Right $ eval' (+) x y
    Sub x y -> Right $ eval' (-) x y
    Mul x y -> Right $ eval' (*) x y
    Div x (Lit 0) -> Left "Error: Division by zero"
    Div x y -> Right $ eval' div x y
  where
    eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
    eval' operator a b = operator (eval a) (eval b)
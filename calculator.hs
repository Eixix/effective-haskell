module Calculator where
import Text.Read (readEither)

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
parse' (token:rest) =
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
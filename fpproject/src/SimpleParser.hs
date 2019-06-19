module SimpleParser where

import AST
import Control.Applicative

newtype Parser a = Parser
    { runParser :: String -> (String, Maybe a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
        let (output, ma) = p input
        in (output, f <$> ma)

instance Applicative Parser where
    pure x = Parser $ \input -> (input, Just x)
    (<*>) (Parser ps1) (Parser ps2) = Parser $ \input ->
        let (input', mf) = ps1 input; (output, ma) = ps2 input'
        in (output, mf <*> ma)

instance Monad Parser where
    return = pure
    Parser pa >>= f = Parser $ \ input ->
        case pa input of
            (input', Just a) -> runParser (f a) input'
            (output, Nothing) -> (output, Nothing) 

instance Alternative Parser where
    empty = failure
    (<|>) (Parser ps1) (Parser ps2) = Parser $ \input ->
        case ps1 input of
            (_, Nothing) -> ps2 input
            res -> res

failure :: Parser a
failure = Parser $ \_ -> ([], Nothing)

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> ([], Nothing)
   (c:cs) -> (cs, Just c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= \c ->
    if f c then return c
           else failure

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

parseBool :: Parser Bool
parseBool = Parser $ \input ->
    if (take 4 input) == "True" then (drop 4 input, Just True)
        else if (take 5 input) == "False" then (drop 5 input, Just False)
            else ([], Nothing)

parseNatural :: Parser Int
parseNatural = Parser $ \input ->
    let r = takeWhile isDigit input
    in if null r then ([], Nothing)
                 else (drop (length r) input, Just (read r :: Int))
    where isDigit x = x >= '0' && x <= '9'

parseInt :: Parser Int
parseInt = Parser $ \input ->
    if null input || (take 1 input) /= "-"
        then runParser parseNatural input
        else let (output, res) = runParser parseNatural (drop 1 input)
             in case res of
                Just x -> (output, Just $ -x)
                Nothing -> (output, Nothing)

parseChar :: Parser Char
parseChar = Parser $ \input ->
    case input of
        (x:xs) -> (xs, Just x)
        _ -> ([], Nothing)

parseCharC :: Char -> Parser Char
parseCharC c = Parser $ \input ->
    case input of
        (x:xs) | x==c -> (xs, Just c)
        _ -> ([], Nothing)

parseString :: String -> Parser String
parseString str = mapM parseCharC str

parseSpace :: Parser String
parseSpace = many $ oneOf " \n\t\r"

parseExprBool :: Parser Expr
parseExprBool = do
    parseString "Bool"
    parseSpace
    b <- parseBool
    return $ EBoolLit b

parseExprInt :: Parser Expr
parseExprInt = do
    parseString "Int"
    parseSpace
    x <- parseInt
    return $ EIntLit x

parseExprChar :: Parser Expr
parseExprChar = do
    parseString "Char"
    parseSpace
    parseCharC '\''
    c <- parseChar
    parseCharC '\''
    return $ ECharLit c

parseExprAdd :: Parser Expr
parseExprAdd = do
    parseCharC '+'
    parseSpace
    e1 <- parseExpr
    parseSpace
    e2 <- parseExpr
    return $ EAdd e1 e2

parseExpr :: Parser Expr
parseExpr = do
    parseCharC '('
    parseSpace
    res <- parseExprBool <|>
           parseExprInt  <|>
           parseExprChar <|>
           parseExprAdd
    parseSpace
    parseCharC ')'
    return $ res
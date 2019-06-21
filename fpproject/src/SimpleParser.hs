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
parseString = mapM parseCharC

-- >=1 spaces
parseSpace :: Parser String
parseSpace = some $ oneOf " \n\t\r"

-- >=0 spaces
parseMaybeSpace :: Parser String
parseMaybeSpace = many $ oneOf " \n\t\r"

parseName :: Parser String
parseName = some $ satisfy (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '\'' || c == '_')

parseVarName :: Parser String
parseVarName = do
    parseCharC '"'
    s <- parseName
    parseCharC '"'
    return s

parseTypeArrow :: Parser Type
parseTypeArrow = do
    parseString "TArrow"
    parseSpace
    t1 <- parseType
    parseSpace
    t2 <- parseType
    return $ TArrow t1 t2 

parseTypeBool :: Parser Type
parseTypeBool = do
    parseString "TBool"
    return $ TBool

parseTypeInt :: Parser Type
parseTypeInt = do
    parseString "TInt"
    return $ TInt

parseTypeChar :: Parser Type
parseTypeChar = do
    parseString "TChar"
    return $ TChar

parseType2 :: Parser Type
parseType2 = do
    parseCharC '('
    parseMaybeSpace
    res <- parseType
    parseMaybeSpace
    parseCharC ')'
    return res

parseType :: Parser Type
parseType = do
    res <- parseType2    <|>
           parseTypeBool <|>
           parseTypeInt  <|>
           parseTypeChar <|>
           parseTypeArrow
    return res

parseExprBool :: Parser Expr
parseExprBool = do
    parseString "EBoolLit"
    parseSpace
    b <- parseBool
    return $ EBoolLit b

parseExprInt :: Parser Expr
parseExprInt = do
    parseString "EIntLit"
    parseSpace
    x <- parseInt
    return $ EIntLit x

parseExprChar :: Parser Expr
parseExprChar = do
    parseString "ECharLit"
    parseSpace
    parseCharC '\''
    c <- parseChar
    parseCharC '\''
    return $ ECharLit c

parseExprNot :: Parser Expr
parseExprNot = do
    parseString "ENot"
    parseSpace
    e <- parseExpr
    return $ ENot e

parseExprAlgebra4 :: Parser Expr
parseExprAlgebra4 = do
    c0 <- parseChar
    c1 <- parseChar
    c2 <- parseChar
    c3 <- parseChar
    parseSpace
    e1 <- parseExpr
    parseSpace
    e2 <- parseExpr
    case [c0,c1,c2,c3] of
        "EAdd" -> return $ EAdd e1 e2
        "ESub" -> return $ ESub e1 e2
        "EMul" -> return $ EMul e1 e2
        "EDiv" -> return $ EDiv e1 e2
        "EMod" -> return $ EMod e1 e2
        "ENeq" -> return $ ENeq e1 e2
        "EAnd" -> return $ EAnd e1 e2
        _     -> failure

parseExprAlgebra3 :: Parser Expr
parseExprAlgebra3 = do
    c0 <- parseChar
    c1 <- parseChar
    c2 <- parseChar
    parseSpace
    e1 <- parseExpr
    parseSpace
    e2 <- parseExpr
    case [c0,c1,c2] of
        "EOr" -> return $ EOr e1 e2
        "EEq" -> return $ EEq e1 e2
        "ELt" -> return $ ELt e1 e2
        "ELe" -> return $ ELe e1 e2
        "EGt" -> return $ EGt e1 e2
        "EGe" -> return $ EGe e1 e2
        _    -> failure

-- EIf e1 e2 e3
parseExprIf :: Parser Expr
parseExprIf = do
    parseString "EIf"
    parseMaybeSpace
    e1 <- parseExpr
    parseMaybeSpace
    e2 <- parseExpr
    parseMaybeSpace
    e3 <- parseExpr
    return $ EIf e1 e2 e3

-- EVar var_name
parseExprVar :: Parser Expr
parseExprVar = do
    parseString "EVar"
    parseSpace
    s <- parseVarName
    return $ EVar s

-- ELet (s, e1) e2
parseExprLet :: Parser Expr
parseExprLet = do
    parseString "ELet"
    parseMaybeSpace
    parseCharC '('
    parseMaybeSpace
    s <- parseVarName
    parseMaybeSpace
    parseCharC ','
    parseMaybeSpace
    e1 <- parseExpr
    parseMaybeSpace
    parseCharC ')'
    parseMaybeSpace
    e2 <- parseExpr
    return $ ELet (s, e1) e2

-- ELambda (String, Type) Expr
-- ELambda (s, type) e
parseExprLambda :: Parser Expr
parseExprLambda = do
    parseString "ELambda"
    parseMaybeSpace
    parseCharC '('
    parseMaybeSpace
    s <- parseVarName
    parseMaybeSpace
    parseCharC ','
    parseMaybeSpace
    t <- parseType
    parseMaybeSpace
    parseCharC ')'
    parseMaybeSpace
    e <- parseExpr
    return $ ELambda (s, t) e

-- EApply Expr Expr
-- EApply e1 e2
parseExprApply :: Parser Expr
parseExprApply = do
    parseString "EApply"
    parseMaybeSpace
    e1 <- parseExpr
    parseMaybeSpace
    e2 <- parseExpr
    return $ EApply e1 e2

-- ELetRec String (String, Type) (Expr, Type) Expr
-- ELetRec f (x, tx) (e1, ty) e2
parseExprLetRec :: Parser Expr
parseExprLetRec = do
    parseString "ELetRec"
    parseSpace
    f <- parseVarName
    parseMaybeSpace
    -- (x, tx)
    parseCharC '('
    parseMaybeSpace
    x <- parseVarName
    parseMaybeSpace
    parseCharC ','
    parseMaybeSpace
    tx <- parseType
    parseMaybeSpace
    parseCharC ')'
    parseMaybeSpace
    -- (e1, ty)
    parseCharC '('
    parseMaybeSpace
    e1 <- parseExpr
    parseMaybeSpace
    parseCharC ','
    parseMaybeSpace
    ty <- parseType
    parseMaybeSpace
    parseCharC ')'

    parseMaybeSpace
    e2 <- parseExpr
    return $ ELetRec f (x, tx) (e1, ty) e2

-- (Expr)
parseExpr2 :: Parser Expr
parseExpr2 = do
    parseCharC '('
    parseMaybeSpace
    res <- parseExpr
    parseMaybeSpace
    parseCharC ')'
    return res

parseExpr :: Parser Expr
parseExpr = do
    res <- parseExpr2        <|>
           parseExprBool     <|>
           parseExprInt      <|>
           parseExprChar     <|>
           parseExprNot      <|>
           parseExprAlgebra3 <|>
           parseExprAlgebra4 <|>
           parseExprIf       <|>
           parseExprVar      <|>
           parseExprLet      <|>
           parseExprLambda   <|>
           parseExprApply    <|>
           parseExprLetRec
    -- parseMaybeSpace
    return res

-- EBind String Expr
parseBind :: Parser (String, Expr)
parseBind = do
    parseString "EBind"
    parseSpace
    s <- parseVarName
    parseSpace
    e <- parseExpr
    return $ (s, e)

parseProgram :: Parser Program
parseProgram = do
    parseString "Program"
    parseMaybeSpace
    parseString "[]"
    parseMaybeSpace
    e <- parseExpr
    parseMaybeSpace
    return $ Program [] e

-- :t program
parseGetType :: Parser Program
parseGetType = do
    parseString ":t"
    parseSpace
    p <- parseProgram
    parseMaybeSpace
    return $ p
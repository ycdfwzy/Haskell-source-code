-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State
import Debug.Trace
import qualified Data.Map.Strict as Map

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VExpr Expr
  -- ... more
  deriving (Show, Eq)

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
  vars :: Map.Map String Value,
  apps :: [Value]
                          } deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

isSameType :: Value -> Value -> ContextState Bool
isSameType v1 v2 = do
  case v1 of
    VInt _ -> case v2 of VInt _ -> return True
                         _ -> return False
    VBool _ -> case v2 of VBool _ -> return True
                          _ -> return False
    VChar _ -> case v2 of VChar _ -> return True
                          _ -> return False
    _ -> return False

isInt :: Value -> ContextState Bool
isInt v = do
  case v of
    VInt _ -> return True
    _ -> return False

isChar :: Value -> ContextState Bool
isChar v = do
  case v of
    VChar _ -> return True
    _ -> return False

isBool :: Value -> ContextState Bool
isBool v = do
  case v of
    VBool _ -> return True
    _ -> return False

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

getInt :: Expr -> ContextState Int
getInt e = do
  ev <- eval e
  case ev of
    VInt i -> return i
    _ -> lift Nothing

evalAnd :: Expr -> Expr -> ContextState Bool
evalAnd e1 e2 = do
  v1 <- getBool e1
  case v1 of
    True -> getBool e2
    False -> return False

evalOr :: Expr -> Expr -> ContextState Bool
evalOr e1 e2 = do
  v1 <- getBool e1
  case v1 of
    True -> return True
    False -> getBool e2

evalNumeric :: Expr -> Expr -> (Int -> Int -> Int) -> ContextState Int
evalNumeric e1 e2 op = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return $ op ev1 ev2

evalEq :: Expr -> Expr -> ContextState Bool
evalEq e1 e2 =
  eval e1 >>= \ev1 ->
    eval e2 >>= \ev2 -> do
      sameType <- isSameType ev1 ev2
      if sameType && ev1 == ev2 then return True else return False

evalOrd :: Expr -> Expr -> (Char -> Char -> Bool) -> (Int -> Int -> Bool) -> ContextState Bool
evalOrd e1 e2 opC opI =
  eval e1 >>= \ev1 ->
    eval e2 >>= \ev2 -> do
      sameType <- isSameType ev1 ev2
      isC <- isChar ev1
      isI <- isInt ev1
      if not sameType then return False
        else if isC then let (VChar x)=ev1 in let (VChar y)=ev2 in return (opC x y)
          else if isI then let (VInt x)=ev1 in let (VInt y)=ev2 in return (opI x y)
            else return False

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit i) = return $ VInt i
eval (ECharLit c) = return $ VChar c
eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)
eval (EAnd e1 e2) = evalAnd e1 e2 >>= \b -> return (VBool b)
eval (EOr e1 e2) = evalOr e1 e2 >>= \b -> return (VBool b)
eval (EAdd e1 e2) = evalNumeric e1 e2 (+) >>= \b -> return (VInt b)
eval (ESub e1 e2) = evalNumeric e1 e2 (-) >>= \b -> return (VInt b)
eval (EMul e1 e2) = evalNumeric e1 e2 (*) >>= \b -> return (VInt b)
eval (EDiv e1 e2) = evalNumeric e1 e2 div >>= \b -> return (VInt b)
eval (EMod e1 e2) = evalNumeric e1 e2 mod >>= \b -> return (VInt b)
eval (EEq e1 e2) = evalEq e1 e2 >>= \b -> return (VBool b)
eval (ENeq e1 e2) = evalEq e1 e2 >>= \b -> return (VBool $ not b)
eval (ELt e1 e2) = evalOrd e1 e2 (<) (<) >>= \b -> return (VBool b)
eval (EGt e1 e2) = evalOrd e1 e2 (>) (>) >>= \b -> return (VBool b)
eval (ELe e1 e2) = evalOrd e1 e2 (<=) (<=) >>= \b -> return (VBool b)
eval (EGe e1 e2) = evalOrd e1 e2 (>=) (>=) >>= \b -> return (VBool b)
eval (EIf e1 e2 e3) = getBool e1 >>= \b -> if b then eval e2 else eval e3
eval (EVar s) = do
  ctx <- get
  case (Map.!?) (vars ctx) s of
    Just (VExpr e) -> eval e
    Just x -> return x
    _ -> return $ VExpr (EVar s)
eval (ELambda (pn, pt) e) = do
  ctx <- get
  trace "Lambda:" $ trace pn $ trace (show e) $
    case (apps ctx) of
      (x:xs) -> do
                  put Context { vars = (Map.insert pn x $ vars ctx), apps = xs }
                  res <- eval e
                  put ctx
                  trace (show x) $ trace (show res) $ return res
      _ -> do
            put Context { vars = (Map.delete pn $ vars ctx), apps = (apps ctx) }
            res <- eval e
            put ctx
            case res of
              VExpr expr -> return $ VExpr (ELambda (pn, pt) expr)
              _ -> return res
eval (ELet (n, e1) e2) = do
  ev1 <- eval e1
  ctx <- get
  put Context { vars = (Map.insert n ev1 $ vars ctx), apps = (apps ctx) }
  res <- eval e2
  put ctx
  trace "LET:" $ trace (n ++ (' ':(show e1))) $ trace (show e2) return res
-- eval (ELetRec f (x, tx) (e1, ty) e2) = undefined
eval (EApply e1 e2) = do
  ev2 <- eval e2
  ctx <- get
  put Context { vars = (vars ctx), apps = (ev2 : (apps ctx)) }
  res <- eval e1
  put ctx
  trace "APPLY:" $ trace (show e1) $ trace (show e2) $ trace (show ev2) $ trace (show (vars ctx)) $ trace (show res) $ return res
eval _ = undefined

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context { vars=Map.empty, apps=[] } -- 可以用某种方式定义上下文，用于记录变量绑定状态


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  Just (VExpr e) -> RExpr e
  _ -> RInvalid

-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State
import qualified Data.Map.Strict as Map

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VLbd Expr (Map.Map String Value) String
  deriving (Show, Eq)

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
  vars :: Map.Map String Value
                       } deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

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
  ev1 <- getBool e1
  case ev1 of
    True -> getBool e2
    _ -> return False

evalOr :: Expr -> Expr -> ContextState Bool
evalOr e1 e2 = do
  ev1 <- getBool e1
  case ev1 of
    True -> return True
    _ -> getBool e2

evalEq :: Expr -> Expr -> ContextState Bool
evalEq e1 e2 = do
  eval e1 >>= \ev1 ->
    case ev1 of
      VLbd _ _ _ -> lift Nothing
      _ -> do
            eval e2 >>= \ev2 ->
              case ev2 of
                VLbd _ _ _ -> lift Nothing
                _ -> return $ ev1 == ev2

evalOrd :: (Int -> Int -> Bool) -> (Char -> Char -> Bool) -> Expr -> Expr -> ContextState Bool
evalOrd opI opC e1 e2 = do
  eval e1 >>= \ev1 ->
    eval e2 >>= \ev2 ->
      case ev1 of
        VInt x -> case ev2 of
                    VInt y -> return $ opI x y
                    _ -> return False
        VChar x-> case ev2 of
                    VChar y -> return $ opC x y
                    _ -> return False
        _ -> return False

evalNumeric :: (Int -> Int -> Int) -> Expr -> Expr -> ContextState Int
evalNumeric op e1 e2 = do
  ev1 <- getInt e1
  ev2 <- getInt e2
  return $ op ev1 ev2

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit i) = return $ VInt i
eval (ECharLit c) = return $ VChar c
eval (ENot e) = getBool e >>= \b -> return (VBool $ not b)
eval (EAnd e1 e2) = evalAnd e1 e2 >>= \b -> return (VBool b)
eval (EOr e1 e2) = evalOr e1 e2 >>= \b -> return (VBool b)
eval (EEq e1 e2) = evalEq e1 e2 >>= \b -> return (VBool b)
eval (ENeq e1 e2) = evalEq e1 e2 >>= \b -> return (VBool $ not b)
eval (ELt e1 e2) = evalOrd (<) (<) e1 e2 >>= \b -> return (VBool b)
eval (EGt e1 e2) = evalOrd (>) (>) e1 e2 >>= \b -> return (VBool b)
eval (ELe e1 e2) = evalOrd (<=) (<=) e1 e2 >>= \b -> return (VBool b)
eval (EGe e1 e2) = evalOrd (>=) (>=) e1 e2 >>= \b -> return (VBool b)
eval (EAdd e1 e2) = evalNumeric (+) e1 e2 >>= \i -> return (VInt i)
eval (ESub e1 e2) = evalNumeric (-) e1 e2 >>= \i -> return (VInt i)
eval (EMul e1 e2) = evalNumeric (*) e1 e2 >>= \i -> return (VInt i)
eval (EDiv e1 e2) = evalNumeric div e1 e2 >>= \i -> return (VInt i)
eval (EMod e1 e2) = evalNumeric mod e1 e2 >>= \i -> return (VInt i)
eval (EIf e1 e2 e3) = getBool e1 >>= \b -> if b then eval e2 else eval e3
eval (EVar n) = do
  ctx <- get
  case ((Map.!?) (vars ctx) n) of
    Just x -> return x
    _ -> lift Nothing
eval (ELambda (pn, pt) e) = return $ (VLbd e Map.empty pn)
eval (ELet (n, e1) e2) = do
  ev1 <- eval e1
  ctx <- get
  put Context { vars = (Map.insert n ev1 $ vars ctx) }
  res <- eval e2
  put ctx
  return res
eval (ELetRec f (x, tx) (e1, ty) e2) = do
  ctx <- get
  put Context { vars = (Map.insert f (VLbd e1 Map.empty x) $ vars ctx) }
  res <- eval e2
  put ctx
  return res
eval (EApply e1 e2) = do
  ev1 <- eval e1
  ev2 <- eval e2
  case ev1 of
    VLbd expr ms s -> case expr of
                        ELambda _ _ -> eval expr >>= \ev -> let (VLbd e _ sy)=ev in return (VLbd e (Map.insert s ev2 ms) sy)
                        _ -> do
                          ctx <- get
                          put Context { vars = (Map.insert s ev2 (Map.union ms $ vars ctx)) }
                          res <- eval expr
                          put ctx
                          return res
    x -> lift Nothing
-- ... more
eval _ = undefined

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context { vars=Map.empty } -- 可以用某种方式定义上下文，用于记录变量绑定状态


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
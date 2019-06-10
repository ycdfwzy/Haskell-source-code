-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State
import qualified Data.Map.Strict as Map

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
  vars :: Map.Map String Type
                       }
  deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
  et <- eval e
  case et of
    TInt -> return TInt
    _ -> lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
  et <- eval e
  case et of
    TChar -> return TChar
    _ -> lift Nothing

sameType :: Type -> Type -> ContextState Type
sameType t1 t2 = do
  if t1 == t2 then return TBool else lift Nothing

isSameType :: Expr -> Expr -> ContextState Type
isSameType e1 e2 = do
  et1 <- eval e1
  et2 <- eval e2
  sameType et1 et2 >> return et1

evalOrd :: Expr -> Expr -> ContextState Type
evalOrd e1 e2 = do
  et <- isSameType e1 e2
  case et of
    TInt -> return TBool
    TChar -> return TBool
    _ -> lift Nothing

evalEq :: Expr -> Expr -> ContextState Type
evalEq e1 e2 = isSameType e1 e2 >> return TBool

evalVar :: Map.Map String Type -> String -> ContextState Type
evalVar mp s = do
  case ((Map.!?) mp s) of
    Just x -> return x
    _ -> lift Nothing

eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (EIntLit _) = return TInt
eval (ECharLit _) = return TChar
eval (ENot e) = isBool e >> return TBool
eval (EAnd e1 e2) = isBool e1 >> (isBool e2 >> return TBool)
eval (EOr e1 e2) = isBool e1 >> (isBool e2 >> return TBool)
eval (EAdd e1 e2) = isInt e1 >> (isInt e2 >> return TInt)
eval (ESub e1 e2) = isInt e1 >> (isInt e2 >> return TInt)
eval (EMul e1 e2) = isInt e1 >> (isInt e2 >> return TInt)
eval (EDiv e1 e2) = isInt e1 >> (isInt e2 >> return TInt)
eval (EMod e1 e2) = isInt e1 >> (isInt e2 >> return TInt)
eval (EEq e1 e2) = evalEq e1 e2
eval (ENeq e1 e2) = evalEq e1 e2
eval (ELt e1 e2) = evalOrd e1 e2
eval (EGt e1 e2) = evalOrd e1 e2
eval (ELe e1 e2) = evalOrd e1 e2
eval (EGe e1 e2) = evalOrd e1 e2
eval (EIf e1 e2 e3) = isBool e1 >> isSameType e2 e3
eval (EVar s) = do
  ctx <- get
  evalVar (vars ctx) s
eval (ELambda (pn, pt) e) = do
  ctx <- get
  put $ Context {vars=(Map.insert pn pt $ vars ctx)}
  result <- eval e
  put ctx
  return (TArrow pt result)
eval (ELet (n, e1) e2) = do
  et <- eval e1
  ctx <- get
  put $ Context {vars=(Map.insert n et $ vars ctx)}
  result <- eval e2
  put ctx
  return result
eval (ELetRec f (x, tx) (e1, ty) e2) = do
  ctx <- get
  put $ Context {vars=(Map.insert f (TArrow tx ty) $ Map.insert x tx $ vars ctx)}
  res1 <- eval e1
  put $ Context {vars=(Map.insert f (TArrow tx ty) $ vars ctx)}
  res2 <- eval e2
  put ctx
  sameType res1 ty >> return res2
eval (EApply e1 e2) = do
  et1 <- eval e1
  et2 <- eval e2
  case et1 of
    (TArrow x y) -> sameType x et2 >> return y
    _ -> lift Nothing
eval _ = undefined


evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context { vars=Map.empty } -- 可以用某种方式定义上下文，用于记录变量绑定状态

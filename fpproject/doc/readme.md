# Haskell大作业说明文档

## 基础功能
直接运行`stack test`测试即可

## Parser

加载`SimpleParser.hs`，语法结构和AST中定义的相同。调用`runParser parseProgram <input_str>`可以得到解析结果。下面是一个例子

```haskell
input = Program [] $ ELetRec
    "fact"
    ("x", TInt)
    (
        EIf (EEq (EVar "x") (EIntLit 0))
            (EIntLit 1)
            (EMul
            (EVar "x")
            (EApply
                (EVar "fact")
                (ESub (EVar "x") (EIntLit 1))
            )
            ),
        TInt)
    (EApply (EVar "fact") (EIntLit 5))
runParser parseProgram $ show input
-- ("",Just (Program [] (ELetRec "fact" ("x",TInt) (EIf (EEq (EVar "x") (EIntLit 0)) (EIntLit 1) (EMul (EVar "x") (EApply (EVar "fact") (ESub (EVar "x") (EIntLit 1)))),TInt) (EApply (EVar "fact") (EIntLit 5)))))
```

## REPL

加载`SimpleREPL.hs`，调用`SimpleREPL.repl`函数进入交互环境。交互环境支持对`Expr`绑定变量，对`Program`类型推导或者求值

```haskell
-- x = 40 + 2
REPL> EBind "x" EAdd (EIntLit 40) (EIntLit 2)
-- evalValue x+9
REPL> Program [] EAdd (EVar "x") (EIntLit 9)
RInt 51
-- evalType \x->x+1
REPL> :t Program [] ELambda ("x",TInt) (EAdd (EVar "x") (EIntLit 1))
TArrow TInt TInt
-- evalValue let fact = (\x -> if x == 0 then 1 else x * fact (x-1)) :: Int -> Int in fact 5
REPL> Program [] ELetRec "fact" ("x", TInt) (EIf (EEq (EVar "x") (EIntLit 0)) (EIntLit 1) (EMul (EVar "x") (EApply (EVar "fact") (ESub (EVar "x") (EIntLit 1)))),TInt)  (EApply (EVar "fact") (EIntLit 5))
RInt 120
```

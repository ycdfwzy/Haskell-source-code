# Haskell大作业报告

## EvalType

`evalType2`接受两个参数，一个是初始的上下文（`Data.Map.Strict.Map`）,一个待推导的`Program`，返回值是包在`Maybe`里的`Type`。`evalType`只接受一个参数，他等价于`evalType2 Map.empty`

在求类型的过程中，遇到类似`ELambda`，`ELet`这样的语句，需要更新上下文，就用类似于下面的结构
```Haskell
do
    ctx <- get  -- save current context
    put $ Contest {...} -- update context
    ... -- eval type
    put ctx -- recover context
```
在有类型约束的情况下，会检查类型是否合法。例如，在`EApply e1 e2`中，设`e2`的类型是`T2`，那么`e1`的类型一定是`T2->T`，否则类型推导失败。

## EvalValue

`evalValue2`接受两个参数，一个是上下文（`Map.Map String Value`），一个待求值的`Program`，如果求值结果是`Bool,Char,Int`中的一个，那么返回对应的结果，否则返回`RInvalid`。`evalValue`只接受一个参数，他等价于`evalValue2 Map.empty`。

为了方便求值，在`Value`中引入`VLbd`类型，他有三个值域`Expr, Map, String`，分别存储`VLbd`的主体`Expr`，已存的参数列表`Map`，以及还未存的变量`String`。如果遇到`EApply e1 e2`，正确情况下`e1`的求值结果是`VLbd`，这样就可以将`(String, eval e2)`存入`Map`中，如果`VLbd`中的`Expr`是`ELambda`，那么递归求值下去，否则可以将`Map`加入到上下文中，具体求值。

## SimpleParser

语法结构和AST中定义的相同，即对于一个程序`Program [] expr`，调用`show $ Program [] expr`就得到可以被`Parser`解析的字符串。

实现了一个`Parser`类型，内部是一个`String->(String, Maybe a)`的函数，即接受一个字符串，将解析结果返回。依次将`Parser`实例化为`Functor, Applicative, Monad, Alternative`。

各条语句的解析方式几乎无差，这里以解析`Program`的过程为例，讲解见注释：

```Haskell
parseProgram :: Parser Program
parseProgram = do
    parseString "Program" -- parse phrase 'Program'
    parseMaybeSpace       -- zero or more ' ', '\t', '\n' or '\r'
    parseString "[]"      -- parse phrase '[]'
    parseMaybeSpace       -- zero or more ' ', '\t', '\n' or '\r'
    e <- parseExpr        -- parse expression
    parseMaybeSpace       -- zero or more ' ', '\t', '\n' or '\r'
    return $ Program [] e -- construct program and return
```

## SimpleREPL

交互模式支持三种类型的语句：

+ 绑定变量，`EBind "s" expr`。程序在匹配到这个模式之后，对表达式`expr`推到类型、求值，并将结果存储到上下文中。
+ 类型推导，`:t program`。程序在匹配到这个模式之后，将`repl`的全局上下文带入做类型推导（用到了`EvalType.evalType2`）。
+ 求值，`program`。程序在匹配到这个模式之后，将`repl`的全局上下文带入做求值（用到了`EvalValue.evalValue2`）。

其余均为不合法语句，程序会提示语法错误。
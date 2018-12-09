## Higher Order Functions
Q: What is **Higher Order Function**  
A: A function that takes other functions as arguments.

## Partial application
偏函数应用指传入不完全的参数，返回接受剩下参数的新函数。
```hakell
f :: Integer -> (Integer -> Integer)
-- f x y 和 (f x) y 是等价的
-- 函数类型是右结合的，而函数调用是左结合的
```

## Local definitions (局部定义)
1. As let-bindings:
```haskell
cylinder :: (RealFloat a) => a -> a -> a 
cylinder r h =
  let sideArea = 2 * pi * r * h 
    topArea = pi * r ^2
  in sideArea + 2 * topArea
```
`let`的内容仅对`in`部分可见
2. In a where-clause:
```haskell
bmiTell :: (RealFloat a) => a -> a -> String 
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!" 
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
```
个人更喜欢`where`，局部定义里的类型签名通常省略

## A note on indentation
haskell对缩进敏感！  
根据缩进的不同来判断语句的开始和结束，所以务必保证缩进正确，更多规则可以参考[chapter on indentation](https://en.wikibooks.org/wiki/Haskell/Indentation)

## Lambda expressions（匿名函数）
很多时候并不想为每一个函数命名，特别是那些只会被调用一次的函数。
此时可以用lambda表达式来定义匿名函数
```haskell
flip' :: (a -> b -> c) -> b -> a -> c 
flip' f = \x y -> f y x
-- 炫酷，简洁，易懂，破费！
```
就像上面这样，`\`表示这是一个lambda表达式。

## Data types
```haskell
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
-- Thing 类型有五个值构造器(data constructor)

-- ====================================================
-- 可以递归地定义类型
data IntList = Empty | Cons Int IntList
-- 内置的list就是以类似的方式定义的: data [a] = [] | a : [a]

-- ====================================================
-- 可以使用Type为现有的类型设置别名
type String = [Char]

```
类型和值构造器的名字通常以大写字母开头。
## Case Expessions
```haskell
n = case "Hello" of
  []      -> 3
  ('H':s) -> length s
  _       -> 7
-- 定义函数的模式匹配不过是case 表达式的语法题
```

## Some terminology
1. A data type where none of the constructors has parameters is called an *enumeration type*.
2. A data type with exactly one constructor is called a *product type*.
3. A data type with multiple constructors is called a *sum type*.
4. A data type with no constructors is an *empty type*. Yes, that is a thing. Yes, this is sometimes useful. (尚不理解)

## Pure Interaction
> Q: How can we have interaction in a world without side-effects?  
> A: Separate the logic of the state change from the logic of remembering the current state

如果了解`Redux`就会发现两者何其相似！  
纯的`state change`函数就是Reducer.  
`the logic of remembering the current state`就是State。

```haskell
-- interactionOf是“CodeWorld”包提供的一个函数，其签名如下
interactionOf :: world ->
                (Double -> world -> world) ->
                (Event -> world -> world) ->
                (world -> Picture) ->
                IO ()
```
签名中的4个参数对应到 `React&Redux` 架构即：
1. world: defaultState (rootReducer(undefined, {type: 'INIT'}))
2. (Double -> world -> world): timeReducer
3. (Event -> world -> world): eventReducer
4. (world -> Picture): React
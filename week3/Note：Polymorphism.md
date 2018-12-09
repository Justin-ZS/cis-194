## Parametric polymorphism
> Polymorphism is a property of a function that it can work with any type

即函数的签名未指定参数的类型。
举个例子：`id :: a -> a`就是多态的，因为它未指定参数的类型。
**注意**：在面向对象中，此特性就是泛型(generics),在函数式里通常省略为`polymorphism`

## Parametricity
多态函数未指定参数的类型，只有被调用时才知道参数的具体类型。(the caller picks the type)
所以多态函数的实现必须适应任何的传入类型，其函数体只能对参数的共性做操作(操作统一)。
这就是`Parametricity`，即函数将会以一种统一的行为作用于所有的类型。
> Polymorphic code behaves “the same” for any type that you pass it to.

带来的好处：
  1. 让类型擦除成为可能（type erasure）在编译阶段把类型信息抹除，从而提高执行性能。Python的类型信息必须带入运行时。
  2. 限制了多态函数的能力，为了以一种统一的行为作用于所有的类型，多态函数只能操作公共部分。
  > As a user of polymorphic functions, parametricity corresponds not to *restrictions* but to *guarantees*. In general, it is much easier to use and reason about tools when those tools give you strong guarantees as to how they will behave.
  
## Polymorphic Data Types
不仅仅时函数，类型也可以时多态的。
```haskell
-- 之前提到过的List类型
data IntList = Empty | Cons Int IntList
-- 改写成下面这样，t作为类型变量可以是任何类型。
data List t = E | C t (List t)
-- 下面是一些例子。
lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)
```
## Total and Partial Functions
> Functions which have certain inputs that will `make them recurse infinitely` are also called partial. (`or crash`)
Functions which are well-defined on all possible inputs are known as total functions.

在特定输入下会产生死循环或抛出异常的函数是不完全的。
反之，任何情况下都可以正常退出的是完全函数。
例如`head`, `last`函数，在传入空数组时会抛出异常，所以它们是不完全函数！
**最佳实践**： 尽可能的避免使用不完全函数
 1. 完全避免,考虑下面两种写法
    ```haskell
    doStuff1 :: [Int] -> Int
    doStuff1 []  = 0
    doStuff1 [_] = 0
    doStuff1 xs  = head xs + head (tail xs)
    
    doStuff2 :: [Int] -> Int
    doStuff2 []        = 0
    doStuff2 [_]       = 0
    doStuff2 (x1:x2:_) = x1 + x2
    -- 显然第二种更好，以为它是真正完全的。
    ```
  2. 使用`maybe`改写，例如改写`head`函数：
     ```haskell
     safeHead :: [a] -> Maybe a
     safeHead []    = Nothing
     safeHead (x:_) = Just x
     ```
## 1.The `Eq` type class
先看一下（==）的类型：
```haskell
Prelude> :t (==)
(==) :: Eq a => a -> a -> Bool
```
`=>`箭头的左边就是类型约束(`type class constraint`),表示（==）仅能用于Eq类型的成员类型上。
可以从Eq中获取更多信息：
```haskell
Prelude> :info Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
  	-- Defined in ‘GHC.Classes’
instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
instance Eq Word -- Defined in ‘GHC.Classes’
instance Eq Ordering -- Defined in ‘GHC.Classes’
instance Eq Int -- Defined in ‘GHC.Classes’
instance Eq Float -- Defined in ‘GHC.Classes’
instance Eq Double -- Defined in ‘GHC.Classes’
instance Eq Char -- Defined in ‘GHC.Classes’
…
```
- 前三行是Eq类的定义，从中可以一窥定义类的方式：
  类型定义中首先给出类型的名字(这里是Eq)，将其应用到类型变量上（这里是a）。然后是任意数量的方法，定义中只需要给出方法的类型签名，具体的实现将在实例中给出。类型约束会被自动生成，所以也不用写。

## The `Eq Coord` instanace
知道了`Eq`类型就可以定义我们自己的实例：
```haskell
data Coord = C Integer Integer

instance Eq Coord where
  C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2
  c1 /= c2 = not (c1 == c2)
```
以上就是实例的定义：
在`instance`关键字后给出类型名（type class）和指定的数据类型（data type）。在`where`中给出方法的函数定义，可以用guard，pattern等等。
方法的实现不需要类型签名，因为它已经在类定义中确定了。

## Default implementations
很明显的，Eq实例的定义中（／=）方法的实现都是一样的。每次都要写重复的内容会很烦，幸好在定义Eq类时，给出了方法的默认实现（default implement），所以在定义实例的时候就可以省略（／=）方法的定义。
下面是的Eq类定义的[源码](http://hackage.haskell.org/package/ghc-prim-0.4.0.0/docs/src/GHC-Classes.html)
```haskell
class  Eq a  where
    (==), (/=)           :: a -> a -> Bool

    {-# INLINE (/=) #-}
    {-# INLINE (==) #-}
    x /= y               = not (x == y)
    x == y               = not (x /= y)
    {-# MINIMAL (==) | (/=) #-}
```
可以通过查看源码来确定是否有默认实现，或者定义实例时直接省略某个方法，如无默认实现，编译器会报错。
## The `Eq Tile` instance
对于某些data type，定义类型起来比较麻烦，比如：
```haskell
data Tile = Wall | Ground | Storage | Box | Blank

instance Eq Tile where
  Wall == Wall = True
  Ground == Ground = True
  Storage == Storage = True
  Box == Box = True
  Blank == Blank = True
  _ == _ = False
-- 每次增加一个值构造器都要修改实例定义。
```
幸好，对于一些基本的type classes，编译器可以自动为我们实现类型。只需要用派生（deriving）：
```haskell
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
```
## Benefits of type classes
- 名字的重载（Overloading of names）  
不必起多个名字。
- 规则（Laws）  
每个类中的方法都需要满足一定的规则。Eq类的规则为：只有a==b为false时，a／=b。
- 泛化的算法（Generic algorithms）  
有时候为特定类型写的函数完全可以泛化，应用到其他类型上，比如：
  ```haskell
  moveFromTo :: Coord -> Coord -> Coord -> Coord
  moveFromTo c1 c2 c | c1 == c   = c2
                     | otherwise = c
  -- 仔细看，就发现其实没有用到Coord类型特有的东西。
  -- 真正的类型签名应该是这个。
  moveFromTo :: Eq a => a -> a -> a -> a
  moveFromTo c1 c2 c | c1 == c   = c2
                     | otherwise = c
  ```
- 实例解析（Instance resolution）
因为我们使用了重载，所以编译器在解析时需要找到队员类型的实例，这有可能是一个递归的过程。这个机制免除了写繁琐代码的困扰。
- 连贯（Coherence）
Haskell确保，对于指定的 `type class` 和指定的 `type`，最多只有一个实例！

## Type classes vs. object-oriented classes
很多从 OOP 走过来的人 们往往会把 `type class` 当成面向对象语言中的 `class` 而感到疑惑，其实它们完全不是一回事！  
如果硬要对应的话，`type class` 对应于Java中的接口：两者都包含没有实现的方法及其类型签名，实例提供实现。
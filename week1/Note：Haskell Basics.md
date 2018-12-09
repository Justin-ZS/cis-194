## What is Haskell?
- **Functional** (Haskell is a *functional* language)

  1. Functions are *first-class*(函数没有特殊之处)
  2. *evaluating expressions* rather than *executing instructions*.(表达式>语句)
- **Pure** (expressions are always *referentially transparent*)

  1. Everything is *immutable*.
  2. Expressions never have “side effects”
  3. Calling the same function with the same arguments results in the same output every time
  4. **Benefits:**
      - Equational reasoning and refactoring
      - Parallelism
      - Fewer headaches
- **Lazy** (expressions are *not evaluated until their results are actually needed*)

  1. It is easy to define a new *control structure* just by defining a function.
  2. It is possible to define and work with *infinite data structures*.
  3. It enables a more *compositional* programming style
  4. **Downside:** 
     - Time and space usage becomes much more complicated!
- **Statically typed** (types are all checked at *compile-time*)

## Themes
- **Types**
  1. Helps clarify thinking and express program structure
  2. Serves as a form of documentation
  3. Turns run-time errors into compile-time errors

- **Abstraction**

  “Don’t Repeat Yourself” is a mantra often heard in the world of programming.
  Taking similar pieces of code and factoring out their commonality is known as the process of abstraction.
  Haskell is very good at abstraction.
  
- **Wholemeal programming**

  > “Functional languages excel at wholemeal programming, a term coined by Geraint Jones. Wholemeal programming means to think big: work with an entire list, rather than a sequence of elements; develop a solution space, rather than an individual solution; imagine a graph, rather than a single path. The wholemeal approach often offers new insights or provides new perspectives on a given problem. It is nicely complemented by the idea of projective programming: first solve a more general problem, then extract the interesting bits and pieces by transforming the general program into more specialised ones.”

## Basic Haskell
- **Declarations and Variables**
```haskell
x :: Int -- `::` is pronounced “has type”
x = 3 -- `=` denotes definition rather than assignment

-- x = 4 “x is defined to be 4” rather then “assign 4 to x”
--Variables are *not mutable boxes*; they are just names for values

main :: IO ()
main = print x

y :: Int
y = y + 1 
-- this does not increment the value of y. Instead, this statement is taken as a recursive definition
```
- **Basic Types**
```haskell
-- Machine-sized integers
i :: Int
i = -78
 {-
 `Int`s are guaranteed by the Haskell language standard to accommodate values at least up to ±2^29, but the exact size depends on your architecture. For example, on many 64-bit architectures the range is ±2^63
 -}

-- Arbitrary-precision integers
n :: Integer
n = 1234567890987654321987340982334987349872349874534

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))
{-
The `Integer` type, on the other hand, is limited only by the amount of memory on your machine
-}

numDigits :: Int
numDigits = length (show reallyBig)

d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4
{- 
For floating-point numbers, there is `Double`, There is also a single-precision floating point type, `Float`, but it is not used much.
-}

-- Booleans
b1, b2 :: Bool
b1 = True
b2 = False

-- Unicode characters
c1, c2, c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'

-- Strings are lists of characters with special syntax
s :: String
s = "Hello, Haskell!"
```
- **Arithmetic**
```haskell
ex01 = 3 + 2
ex02 = 19 - 27
ex03 = 2.35 * 8.6
ex04 = 8.7 / 3.1
ex05 = mod 19 3
ex06 = 19 `mod` 3 -- `backticks` make a function name into an infix operator.
ex07 = 7 ^ 222
ex08 = (-3) * (-7)

-- fromIntegral: converts from any integral type (Int or Integer) to any other numeric type.
-- round, floor, ceiling: convert floating-point numbers to Int or Integer.
```
- **Boolean Logic**
```haskell
ex11 = True && False -- logical and
ex12 = not (False || True) -- logical or
Things can be compared for equality with (==) and (/=), or compared for order using (<),  (>), (<=), and (>=).

ex13 = ('a' == 'a') -- equal
ex14 = (16 /= 3) -- not equal
ex15 = (5 > 3) && ('p' <= 'q') -- 
ex16 = "Haskell" > "C++"
```
Haskell also has `if` expressions: `if b then t else f` is an expression which evaluates to `t` if the Boolean expression `b` evaluates to `True`, and `f` if `b` evaluates to `False`.With an `if` expression, on the other hand, the `else` part is required, since the `if` expression must result in some value.

## Defining Basic Functions
1. **Pattern matching**
```haskell
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
```
从上到下依次匹配，一旦匹配成功，则执行对应函数体

2. **Guards**
```haskell
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1
```
guards模式类似if-else语句，最后一个往往是otherwise。

3. 两个模式可以混合起来用：
```haskell
foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0           = 0
  | n `mod` 17 == 2 = -43
  | otherwise       = n + 3
```
4: 其他：
*Function application binds tighter than any binary operators.*
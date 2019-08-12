import Text.Show
import Control.Monad
import Control.Applicative
-- Exercise 1: Fibonacci numbers

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

-- https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_sec_3.5.2
-- (define fibs
--   (cons-stream 0
--                (cons-stream 1
--                             (add-streams (stream-cdr fibs)
--                                          fibs))))
fibs2 :: [Integer]
fibs2 = 1 : 1 : (zipWith (+) fibs2 (tail fibs2))

-- Exercise 2: Streams

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show a = show (take 20 (streamToList a))
  
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Cons x xs) = Cons (fn x) (streamMap fn xs)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate fn x = Cons x (streamIterate fn (fn x))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys =  Cons x (streamInterleave ys xs)

nats :: Stream Integer
nats = streamIterate (+1) 0

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+1) ruler)


-- Exercise 3: The Supply monad
-- 我都看傻了,这不看答案怎么做的出来。
data Supply s a = S (Stream s -> (a, Stream s))

-- idiom1 :: Supply s a
-- idiom1 = S (\xs -> ）

-- idiom2 :: Supply s a
-- idiom2 = S go
--   where go xs = …
  
get :: Supply s s
get = S (\(Cons x xs) -> (x, xs))

pureSupply :: a -> Supply s a
pureSupply x = S (\xs -> (x, xs))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply fn (S fa) = S go
  where go xs = let (x, xs') = fa xs
                in (fn x, xs')

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 fn (S f1) (S f2) = S go
  where go xs = let (a, xs') = f1 xs
                    (b, xs'') = f2 xs'
                in (fn a b, xs'')
                
bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S f1) fn = S go
  where go xs = let (x, xs') = f1 xs
                    (S f2) = fn x
                in f2 xs

                
runSupply :: Stream s -> Supply s a -> a
runSupply xs (S fn) = fst (fn xs)
  
instance Functor (Supply s) where
    fmap = mapSupply

instance Applicative (Supply s) where
    pure = pureSupply
    (<*>) = mapSupply2 id

instance Monad (Supply s) where
    return = pureSupply
    (>>=) = bindSupply
    
    
data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Node t1 t2) = Node <$> go t1 <*> go t2
    go (Leaf _) = Leaf <$> get

-- *Main> let t = let l = Leaf () ; n = Node in n (n (n l l) l) (n l l)
-- *Main> labelTree t
-- Node (Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

-- 事后诸葛亮：
-- Q：0,1,2 这些值哪来的？
-- A：由 get 函数得到
-- Q：get 被谁调用呢？
-- A：被 runSupply 函数调用
-- Q：为啥是 0,1,2,而不是0,0,0呢？
-- A：看 mapSupply2 函数,fn a b 中的 fn 就是 Node, a,b 即 nats 中连续的值。
-- 把 :77 改成 (b, xs'') = f2 xs 就是 0,0,0,0
-- Q：那为啥不是 0,1,0,1 嘞？
-- A：mapSupply2 返回的是 (fn a b, xs''), xs'' 即消费过的 stream
-- 把 :78 改成 (fn a b, xs) 就是 0,1,0,1


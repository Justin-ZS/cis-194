## Strict evaluation
> Under a *strict evaluation* strategy, function arguments are completely evaluated *before* passing them to the function

简单说，在执行函数体前，对所有的参数求值。  
大多数的语言使用的都是这个策略。（C++, Java, Js）
> The *benefit* of strict evaluation is that it is easy to predict *when and in what order* things will happen.

严格求值的好处就是清晰，可以很明确知道求值这一步在哪被执行的，并且彼此之间的顺序也很明确(一般都是从左到右)，譬如说下面这个函数：
```
f (release_monkeys(), increment_counter())
```
很清楚的知道：在执行函数`f`前会依次执行`release_monkeys()`, `increment_counter()`两个函数，并且把两者的返回值作为参数传给`f`。  
然而如果是在惰性求值的语言里调用此函数，两个函数的执行时机和次序就很模糊了。(取决于f怎么写！)  
这样看来，如果允许写带有副作用的函数，那么严格求值就是唯一选择。
## Side effects and purity
> By “side effect” we mean *anything that causes evaluation of an expression to interact with something outside itself*.

关键在于函数的副作用(尤其是对时间敏感的)。同样的，所以选择了惰性求值，就选择了纯函数

## Lazy evaluation
> Under a lazy evaluation strategy, evaluation of function arguments is *delayed as long as possible*: they are not evaluated until it actually becomes necessary to do so. 

简单说，惰性求值就是尽可能的延迟求值的时间。如果以表达式作为函数参数，就会被打包成一个thunk。

## Pattern matching drives evaluation
参看下面的例子就会明白求值的时机是什么：
```haskell
f1 :: Maybe a -> [Maybe a]
f1 m = [m,m]

f2 :: Maybe a -> [a]
f2 Nothing  = []
f2 (Just x) = [x]
```
`f1`和`f2`拥有相同的函数签名，但是调用前者不会对参数求值，而后者则会对参数求值。
根本的原因在于，`f2`的模式匹配是对参数的值进行匹配，所以需要先求值，再匹配。
这就是模式匹配驱使求值(`pattern matching drives evaluation`)，包含下面两点：
* 表达式仅在被匹配时才会被求值(`Expressions are only evaluated when pattern-matched`）
* 求值到满足模式匹配为止，不会一直求值下去(`…only as far as necessary for the match to proceed, and no farther!`)

## Consequences
惰性求值会带来一些有趣但隐含的结果。
### Purity
如前所述，惰性求值迫使你使用纯函数。
### Understanding space usage
惰性求值的另一个缺点是很难把握程序的空间使用情况。
```haskell
badSum :: Num a => [a] -> a
badSum []     = 0
badSum (x:xs) = x + badSum xs
-- 非尾调用，所以每次迭代都在内存中存着

lazySum :: Num a => [a] -> a
lazySum = go 0
  where go acc []     = acc
        go acc (x:xs) = go (x + acc) xs
-- 尾调用，但是所有的累加都会被延迟到最后一步。
-- 就是这个：(4 + (3 + (2 + (1 + 0))))

strictSum :: Num a => [a] -> a
strictSum = go 0
  where go acc []     = acc
        go acc (x:xs) = acc `seq` go (x + acc) xs
-- `seq` 函数会强迫对第一个参数求值。
```
### Short-circuiting operators
虽然我们说严格求值会预先对表达式求值，但是也有例外，就是逻辑运算符：`&&` or `||`.  
在一些语言里，这也被叫做短路运算符，对于`&&`，如果第一个参数值为`False`，就会直接返回`False`.第二个参数完全不会被动。`||`也是同理。  
这和`if`语句很像。所以在这些语言中，某些情况下可以用简单的`&&`表达式来取代繁琐的`if`语句(比如JS)。
### Infinite data structures
惰性求值最大的一个好处就是可以轻松处理无限的数据结构(`infinite data structures`),比如`repeat 7`就会产生一个只有7的无限list。  
另一个l类似的情况是"可能无限"的数据结构（`“effectively infinite” data structures`）比如用来表示围棋可能状态的树。。。  
这个也很好理解，超过处理上限的大数据，都可以看作是无限。，  
### Pipelining/wholemeal programming
这点说的是在内存利用率上的优势，因为在流水线的数据处理模式下，各个部分可以同步进行，每次只要产生下个部分需要的参数即可。
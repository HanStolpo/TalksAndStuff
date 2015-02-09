---
title: For the Purely Lazy
author: Handré Stolp
date: February 9, 2015
slideLevel: 2
incremental: False
slideVariant: RevealJsSlides
autoStretchCode: False
---

For the Purely Lazy 
=========================

The Low Down 
--------------------------
* Looking at Lazy Evaluation in the purely functional language Haskell.
* All about the lazy, purity just along for the ride.
* Wat is Lazy Evaluation?
* What is it good for?
* What are the gotchas? 

Evaluation strategies
===========================

Eager or Lazy
---------------------------

* In Haskell like languages evaluation is reducing expressions to their simplest form.   
<span style="color:lightblue">```(5 + 4 - 2) * (5 + 4 - 2) ⇒ 49```</span>

* Two options when expression includes function application.  
<span style="color:lightblue">```square (5 + 4 - 2) ⇒ 49```</span>

    * <span style="color:red"> Reduce arguments first (*Innermost reduction / Eager Evaluation*)</span>  
      <span style="color:firebrick"> ```square (5 + 4 - 2) ⇒ square(7) ⇒ 7 * 7 ⇒ 49``` </span>

    * <span style="color:limegreen"> Apply function first  (*Outermost reduction / Lazy Evaluation*)</span>  
      <span style="color:olivedrab"> ```square (5 + 4 - 2) ⇒ (5 + 4 - 2) * (5 + 4 - 2) ⇒ 7 * 7 ⇒ 49``` </span>

* Final answer == expression in normal form  
    <span style="color:lightblue">```49```</span> is the normal form of <span style="color:lightblue">```square (5 + 4 - 2)```</span>
* Eager also called strict.
* Lazy also called non-strict (sort of)
    * Lazy is non-strict but non-strict is not necessarily Lazy.
    * Haskell requires non-strictness
    * Most implementations are Lazy

<div class="notes">
Inner most reduction / outer most reduction from [Thinking Functionally with Haskell][] page 27.
</div>

Lazy is a bit more involved
-----------------------------------
<div style="width: 50%;float: left;">
* Outermost reduction
* Only evaluate as needed
    * Not to normal form 
    * To head normal form / weak head normal form
    * Outer most constructor
* Shares sub expressions when expanding

```haskell
square (5 + 4)
⇒ let x = 5 + 4 in square x
⇒ x * x
⇒ let x = 7 in x * x
⇒ 7*7
⇒ 49
````
```haskell
(5,square (5 + 4))
⇒ let a = 5, b = square (5 + 4) in (a,b)
````
</div>
<div style="width: 50%;float: right;">
![Lazy evaluation showing sharing and evaluating to head normal form](../media/Lazy_Eval_Sharing_and_Head_Normal.svg)
<div>


Whats the difference
--------------------------------
<div style="width: 50%;float: left;">
Given
```haskell 
fst (a, b) = a
fst (0, square (5 + 4)) 
```
Eager
```haskell 
fst (0, square (5 + 4)) 
⇒ fst (0, square 9) 
⇒ fst (0, 9 * 9)
⇒ 0
```
Lazy
```haskell 
fst (0, square (5 + 4)) 
⇒ let a = 0, b = square (5 + 4) in fst (a, b) 
⇒ a
⇒ 0
```
Lazy never evaluated <span style="color:lightblue">```square (5 + 4)```</span> where eager did.
</div>
<div style="width: 50%;float: right;">
![Lazy vs Eager evaluation](../media/Lazy_vs_Eager.svg)
</div>

* * * * * *

<div style="height: 5%;float: top;">
Given
```haskell 
fst (0, ⊥) 
```
</div>
<div style="height: 95%;float: bottom;">
<div style="width: 50%;float: left;">
Eager
```haskell 
fst (0, ⊥) 
⇒ fst ⊥
⇒ ⊥
```
</div>
<div style="width: 50%;float: right;">
Lazy
```haskell 
fst (0, ⊥) 
⇒ let a = 0, b = ⊥ in fst (a, b) 
⇒ a
⇒ 0
```
</div>
</div>
<div style="height: 5%;float: bottom;">
* In the presence of bottom (⊥, undefined / non termination) 
* Lazy evaluation can still return a result
* Lazy version never evaluated the ⊥ argument.
</div>

Lazy always best! Well no.
-----------------------------
<div style="width: 30%;float: left;">
* Lazy evaluation has a bookkeeping overhead
* Unevaluated expression builds up in memory
* Eager is not always better than Lazy
* Lazy is not always better than Eager
* Just a note, technically primitive operations in GHC are not lazy.
</div>
<div style="width: 70%;float: right;">
![The cost of lazy evaluation](../media/Lazy_vs_Eager_Cost.svg)
</div>

The Good, The Bad and The Smugly
====================================

The Good and Bad
-------------------------------------

<div style="width: 50%;float: left;">
### Pros
* More efficient ? 
    * This is a red herring.
    * It could be or it couldn't. 
* Modularity! 
    * This is the real reason
    * John Hughes - [Why Functional Programming Matters][2]
    * Glue allowing composition of functional programs
* Efficient tricks for pure languages.
    * Again ? Contradiction ? Nope.
    * Memoization preserving purity and abstraction.
    * Caching preserving purity and abstraction.
</div>

<div style="width: 50%;float: right;">
### Cons
* Difficult to reason about time and space usage.
    * Time ? Not sold.
        * Lazy O(n) <= Eager O(n)
        * When is the cost? Latency is a concern.
    * Space ?
        * Unfortunately yes.
        * The dreaded space leak.
* Parallel unfriendly
    * Doing work in parallel means doing the work in parallel.
    * Have to force work to be done.
    * Only an issue if you believe in automagic parallelization.
    * See [Parallel and Concurrent Programming in Haskell][3]
</div>

<div class="notes">
[2]: http://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf (Why Functional Programming Matters)
[3]: httP://http://community.haskell.org/~simonmar/pcph/ (Parallel and Concurrent Programming in Haskell: Techniques for Multicore and Multithreaded Programming)
</div>

So Don't be Smug
------------------------------
* Laziness is not a silver bullet.
* Laziness is not a scarlet letter.
* Most eager languages have lazy constructs.
* Most lazy languages have eager constructs.

Modularity
===================

Lazy Aids Composition
---------------------
* From Why [Functional Programming Matters][2]
* Can structured whole programs as function composition
* <span style="color: lightblue">```(f . g) input == f (g input)```</span>
* <span style="color: lightblue">```g```</span> only consumes <span style="color: lightblue">```input```</span> 
  as <span style="color: lightblue">```f```</span> needs it.
* <span style="color: lightblue">```f```</span> knows nothing of <span style="color: lightblue">```g```</span>
* <span style="color: lightblue">```g```</span> knows nothing of <span style="color: lightblue">```f```</span>
* When <span style="color: lightblue">```f```</span> terminates <span style="color: lightblue">```g```</span> terminates
* Allows termination conditions to be separated from loop bodies.
* Can modularise as generators and selectors.

Lazy Composition Sqrt
----------------------
<div style="height: 50%; float: top">
* Example from Why [Functional Programming Matters][2]
* Newton-Raphson to calculate square root approximation.
* One function generates sequence of approximations.
* Two options to chose when approximation is good enough.
    * Don't care approximations to what.
* Can to give different square root approximations.
* Paper expands on examples of Laziness aiding composition.
</div>
<div style="height: 50%; float: bottom">
```haskell
-- Generate a sqrt sequence of operations using Newton-Raphson
nextSqrtApprox n x = (x + n/x) / 2
-- iterate f x == [x, f x, f (f x), ...]
sqrtApprox n = iterate (nextSqrtApprox n) (n/2)
-- element where the difference with previous is below threshold
within eps (a:b:bs) | abs(a-b) <= eps = b
                    | otherwise = within eps (b:bs)
-- Calculate approximate sqrt using within
withinSqrt eps n = within eps (sqrtApprox n)
-- element where ratio with previous is close to 1
relative eps (a:b:bs) | abs(a-b) <= eps * abs b = b
                      | otherwise = relative eps (b:bs)
-- Calculate approximate sqrt using relative
relativeSqrt eps n = within eps (sqrtApprox n)
```
</div>

Caveats about Lazy Composition
------------------------------
* When using Lazy IO
    * Promptness of resource finalization is a problem
* Use libraries libraries like
    * [Conduit][4]
    * [Pipes][5]

[4]: https://hackage.haskell.org/package/conduit (Conduit)
[5]: https://hackage.haskell.org/package/pipes (pipes)

Lazy Tricks
=========================================

Memoization
--------------------------------
<div style="width: 30%; float: left">
* Use Laziness to incrementally build the list of all Fibonacci numbers.
* The list refers to itself to reuse calculations.
* The list is in constant applicative form (CAF) so is memoized
* See [Haskell wiki][5] for more tricks using infinite data structures to
  memoize functions.
</div>
<div style="width: 70%; float: right">
```haskell
fib_list :: [Integer]
fib_list = 0:1:1:2:[n2 + n1| (n2, n1) <- 
                    zip (drop 2 fib_list) (drop 3 fib_list)]
fib_best :: Int -> Integer
fib_best n = fib_list !! n
````
GHCi with timing
````
*Main> 5 < fib_best 100000
True
(0.86 secs, 449258648 bytes)
*Main> 5 < fib_best 100001
True
(0.02 secs, 0 bytes)
*Main> 
````
</div>
[5]: https://wiki.haskell.org/Memoization (Haskell Wiki Memoization)


Caching
-----------------------------------
* So you have some data/results and some expensive queries against it.
* You want to perform the queries only when they are needed.
* You want to perform the queries only once.
* You must preserve purity.
* What do you do?
* Perform the queries and store in the data structure.
* Laziness means they will only be evaluated once and only when needed.

* * * *

* Canned example using really expensive fib.
* ```Fibber``` is some ```Num``` type you can do math on.
* You can ask it for the resultant value of the Fibonacci number.
* You wont export the constructor.

<div style="width: 60%; float: left;">
```{.haskell .stretch}
fib_worst :: Int -> Integer
fib_worst 0 = 0
fib_worst 1 = 1
fib_worst 2 = 1
fib_worst n = fib_worst(n-2) + fib_worst(n-1)

data Fibber = Fibber{fibNum :: Int, fibValue :: Integer}
makeFibber :: Int -> Fibber
makeFibber a = Fibber a (fib_worst a)
instance Eq Fibber where a == b = fibNum a == fibNum b
instance Ord Fibber where a <= b = fibNum a <= fibNum b
instance Show Fibber where show a = show . fibNum $ a
instance Num Fibber where
    a + b = makeFibber (fibNum a + fibNum b)
    a * b = makeFibber (fibNum a * fibNum b)
    abs = makeFibber . abs . fibNum
    signum = makeFibber . signum . fibNum
    fromInteger = makeFibber . fromInteger
    negate = makeFibber . negate . fibNum
```
</div>

<div style="width: 40%; float: right;">
GHCi
```
*Main> let fibber30 = makeFibber 30
(0.00 secs, 0 bytes)
*Main> let fibber25 = makeFibber 25
(0.00 secs, 0 bytes)
*Main> fibValue (fibber30 - fibber25)
5
(0.00 secs, 0 bytes)
*Main> fibValue fibber30
832040
(1.22 secs, 127472744 bytes)
*Main> fibValue fibber30
832040
(0.00 secs, 0 bytes)
*Main> fibValue fibber25
75025
(0.11 secs, 10684624 bytes)
*Main> 
```
</div>

Time and Space
===============================

Times up
-------------------------------
* Algorithmic complexity of Lazy evaluation is never more than Eager.
* Consider Eager behaviour for upper bound.
* Real issue - work might be delayed.
* Delayed work is real issue for parallelism.
* Can always force work using [seq][6] and [deepseq][7] and [force][7]
* See [Parallel and Concurrent Programming in Haskell][3]

[6]: http://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html#v:seq (Base library seq)
[7]: http://hackage.haskell.org/package/deepseq-1.4.0.0/docs/Control-DeepSeq.html (DeepSeq library)

Space Leaks
------------------------
* Space Leak - program / expression uses more memory than required.
    * Memory is eventually released
    * You think it will run in constant memory but it doesn't
    * Building up unevaluated thunks.
    * Unnecessarily keeping reference to data alive.
* Memory leak - program allocates memory that is never reclaimed.
* Pure Haskell code can only be able to Space Leak (no Memory Leak).
* Most other Haskell code should only be able to Space Leak (mostly no Memory Leak).

Examples from "Leaking Space"
--------------------------------
* Some space leak examples from [Leaking Space - Eliminating memory hogs][8]
```haskell
xs = delete "dead" ["alive", "dead"]
```
* Lazy evaluation will keep ```dead``` alive until evaluation of ```xs``` is forced.
* One form of space leak results from adding to and removing from lists but never evaluating (to reduce).
```haskell
xs = let xs' = delete "dead" ["alive", "dead"] in xs' `seq` xs'
```
* Why not always strict/eager.
    * Composition.
    * Composing strictly requires arguments to be evaluated fully.
    * ```sum [1..n]``` will consume O(n) space when evaluated strictly.
    * ```sum [1..n]``` **should** consume O(1) space when evaluated lazily (implementation).
    * Usually easier to introduce strictness when lazy than laziness when strict.

* * *

* This definition is O(n) space. 
* List is not actually kept in memory
* Accumulates ```(+)``` operations. 
```haskell
sum1 (x:xs) = x + sum1 xs 
sum1 [] = 0
```
* This definition is O(n) space. 
* Also accumulates ```(+)``` operations. 
```haskell
sum2 xs = sum2’ 0 xs 
   where 
       sum2’ a (x:xs) = sum2’ (a+x) xs 
       sum2’ a [] = a
```
* This definition is O(1) space. 
```haskell
sum3 xs = sum3’ 0 xs 
   where 
       sum3’ !a (x:xs) = sum3’ (a+x) xs 
       sum3’ !a [] = a
```
* With optimizations in GHC ```sum2``` may be transformed into ```sum3``` during strictness analysis.
* The article has more examples of space leaks.
[8]: http://queue.acm.org/detail.cfm?id=2538488 (Leaking Space - Eliminating memory hogs: By Neil Mitchell)

When Strictness can make things slow
------------------------------------
<div style="float: top;">
* Maybe one should just through strictness in everywhere
* Something I did not know. Pattern matches are strict.
* Example from [Haskell Wiki][9]
* Strict pattern match forces all recursive calls to splitAt
</div>

<div style="float: bottom;">
<div style="width: 50%; float: left;">

```haskell
-- Strict
splitAt_sp n xs = ("splitAt_lp " ++ show n) $
    if n<=0
        then ([], xs)
        else
            case xs of
                [] -> ([], [])
                y:ys ->
                    case splitAt_lp' (n-1) ys of
                        -- pattern match is strict
                        (prefix, suffix) -> (y : prefix, suffix)
```

</div>

<div style="width: 50%; float: right;">

```haskell
-- Lazy
splitAt_lp n xs = ("splitAt_lp " ++ show n) $
    if n<=0
        then ([], xs)
        else
            case xs of
                [] -> ([], [])
                y:ys ->
                    case splitAt_lp' (n-1) ys of
                        -- pattern match is lazy
                        ~(prefix, suffix) -> (y : prefix, suffix)
```

</div>
</div>
<div style="float: bottom;">
GHCi
```
*Main> sum . take 5 . fst . splitAt_sp 10000000 $ repeat 1
5
(20.78 secs, 3642437376 bytes)
*Main> sum . take 5 . fst . splitAt_lp 10000000 $ repeat 1
5
(0.00 secs, 0 bytes)
```
</div>
[9]: https://wiki.haskell.org/Lazy_pattern_match (Haskell Wiki Lazy Pattern Matching)

What to do with thorny space leaks
----------------------------------
* Pinpoint leaks using GHC's profiling tools.
* For some domains libraries exist that eliminate large classes of space leaks by design
    * Streaming libraries like
        * [Conduit][4]
        * [Pipes][5]
    * FRP Libraries like
        * [Netwire][10]

[10]: https://hackage.haskell.org/package/netwire (Netwire)

Further reading
=============

* [How does Lazy Evaluation work?][11] - Great article with lots of pictures.
* [The Point of Laziness][12] - Some criticisms of laziness.
* [Lazy bindings][13] - A response to [The Point of Laziness][12] about what is good about laziness.
* [Leaking Space - Eliminating memory hogs][8] - A good article about space leaks.
* [Reasoning about space leaks with space invariants][14] - Another good article about space leaks.

[11]: https://hackhands.com/lazy-evaluation-works-haskell/
[12]: https://existentialtype.wordpress.com/2011/04/24/
[13]: http://augustss.blogspot.com/2011/05/more-points-for-lazy-evaluation-in.html
[14]: http://apfelmus.nfshost.com/blog/2013/08/21-space-invariants.html

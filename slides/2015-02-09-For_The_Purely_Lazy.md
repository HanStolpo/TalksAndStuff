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
    * [Conduit][3]
    * [Pipes][4]

[3]: https://hackage.haskell.org/package/conduit (Conduit)
[4]: https://hackage.haskell.org/package/pipes (pipes)

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

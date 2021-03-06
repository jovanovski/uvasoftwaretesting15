
Lecture 4: Functional Programming and Imperative Programming

> module Lecture4
> 
> where 
> 
> import Data.List
> import Data.Char
> import Test.QuickCheck

Recall from last time:

> update :: Eq a => (a -> b) -> (a,b) -> a -> b
> update f (x,y) = \ z -> if x == z then y else f z 

> updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
> updates = foldl update

> infixl 1 $$
> 
> ($$) :: a -> (a -> b) -> b
> ($$) = flip ($)

We can use this to implement variable assignment, the basic action in imperative programming.

Define a variable environment (the representation of the computer memory) as a function from variables to appropriate values, let us say integers.

> type Var = String
> type Env = Var -> Integer

To implement variable assignment we need a datatype for expressions, for the assign command assigns an expression to a variable.

> data Expr = I Integer | V Var 
>           | Add Expr Expr 
>           | Subtr Expr Expr 
>           | Mult Expr Expr 
>           deriving (Eq)

Evaluation of an expression in an environment

> eval :: Env -> Expr -> Integer 
> eval _ (I i) = i 
> eval c (V name) = c name
> eval c (Add e1 e2)   = (eval c e1) + (eval c e2)
> eval c (Subtr e1 e2) = (eval c e1) - (eval c e2)
> eval c (Mult e1 e2)  = (eval c e1) * (eval c e2)

Variable Assignment in an Environment

> assign :: Var -> Expr -> Env -> Env 
> assign var expr c = let 
>    value = eval c expr 
>  in 
>    update c (var,value)

Environment initialisation

An environment is a finite object, so it will yield ⊥ (undefined) for all but a finite number of variables.

The initial environment is everywhere undefined:

> initEnv :: Env 
> initEnv = \ _ -> undefined

The Four Ingredients of Imperative Programming

    Variable Assignment: <var> := <expr>

    Conditional Execution: if <bexpr> then <statement1> else <statement2>

    Sequential Composition: <statement1> ; <statement2>

    Iteration: while <expr> do <statement>

These four ingredients make for a Turing complete programming language. A programming language is Turing complete if it is powerful enough to simulate a single taped Turing machine.

It is believed that such languages can express any function that can be computed by an algorithm. This article of faith is called the Church-Turing thesis.

Review Question Why is it impossible to prove the Church-Turing Thesis?

Implementation of While Language in Haskell

Conditions:

> data Condition = Prp Var 
>                | Eq Expr Expr 
>                | Lt Expr Expr 
>                | Gt Expr Expr 
>                | Ng Condition 
>                | Cj [Condition] 
>                | Dj [Condition]
>                deriving (Eq)

Statements:

> data Statement = Ass Var Expr
>                | Cond Condition Statement Statement
>                | Seq [Statement]
>                | While Condition Statement
>                deriving (Eq)

Condition Evaluation

We will interpret basic conditions of the form Prp v by a check whether the value of v is non-zero.

> evalc :: Env -> Condition -> Bool
> evalc env (Prp v)    = env v /= 0
> evalc env (Eq e1 e2) = eval env e1 == eval env e2
> evalc env (Lt e1 e2) = eval env e1 <  eval env e2
> evalc env (Gt e1 e2) = eval env e1 >  eval env e2
> evalc env (Ng c) = not (evalc env c)
> evalc env (Cj cs) = and (map (evalc env) cs)
> evalc env (Dj cs) = or  (map (evalc env) cs)

Statement Execution

Executing a statement of the While language should be an operation that maps environments to environments.

> exec :: Env -> Statement -> Env 
> exec env (Ass v e) = assign v e env
> exec env (Cond c s1 s2) = 
>  if evalc env c then exec env s1 else exec env s2 
> exec env (Seq ss) = foldl exec env ss                    
> exec env w@(While c s) = 
>  if not (evalc env c) then env 
>  else exec (exec env s) w

Example

Imperative program for computing the Fibonacci numbers:

fib n
x := 0; y := 1; 
while n > 0 do { z := x; x := y; y := z+y; n := n-1 }

> fib :: Statement
> fib = Seq [Ass "x" (I 0), Ass "y" (I 1), 
>            While (Gt (V "n") (I 0))
>              (Seq [Ass "z" (V "x"), 
>                    Ass "x" (V "y"),
>                    Ass "y" (Add (V "z") (V "y")), 
>                    Ass "n" (Subtr (V "n") (I 1))])]

To run such programs, we initialize some variables to create an environment, execute a statement, and finally inspect, in the result environment, a list of variables that we are interested in.

> run :: [(Var,Integer)] -> Statement -> [Var] -> [Integer]
> run xs program vars = 
>   exec (updates initEnv xs) program $$ 
>     \ env -> map (eval env) (map V vars)

Now we can run fib:

> runFib n = run [("n",n)] fib ["x"]

Comparison with Functional Version

Here is the definition of while again:

> while :: (a -> Bool) -> (a -> a) -> a -> a
> while = until . (not.)

While + Return

Sometimes it is useful to include a function for transforming the result:

> whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
> whiler p f r = r . while p f 

Now we can do fib in functional imperative style:

> fibonacci :: Integer -> Integer
> fibonacci n = fibon (0,1,n) where 
>   fibon = whiler 
>            (\ (_,_,n) -> n > 0)
>            (\ (x,y,n) -> (y,x+y,n-1))
>            (\ (x,_,_) -> x)

The key to showing the correctness of the imperative version of the Fibonacci algorithm is to show that (x,y) = (F(N-n),F(N-n+1)) holds for the step inside the while loop, where N is the initial value of variable n.

In other words, suppose (x,y) = (F(N-n),F(N-n+1)), and execute the statement (x,y,n) := (y,x+y,n-1). Then afterwards, (x,y) = (F(N-n),F(N-n+1)) holds again.

The functional programmer, instead of checking a loop invariant, proves with induction on k that after the call fb n, fb' is always called with parameters F(n-k), F(n-k+1), k.

We see that proving the loop invariant corresponds to proving the inductive step in the induction proof. In the imperative version we have to deal with three variables x,y,n and in the recursive functional version we reason about three function arguments.

While Loops as Fixpoints

The definition of a fixpoint operation.

> fp :: Eq a => (a -> a) -> a -> a 
> fp f = until (\ x -> x == f x) f

Using this, we can construct another variation on Fibonacci:

> fbo n = (0,1,n) $$ 
>          fp (\ (x,y,k) -> if k == 0 then (x,y,k) else (y,x+y,k-1))

Aside: Fixpoint Calculation of Square Roots

The Babylonian method of computing square roots: repeatedly take the average of an overestimation x and an underestimation a/x to a√.

This works well in Haskell because of the limited precision of floating point numbers.

> bab a = \ x -> ((x + a/x)/2)
> 
> sr a = fp (bab a) a

This gives:

 *Lecture4> sr 2
 1.414213562373095
 *Lecture4> sr 3
 1.7320508075688772
 *Lecture4> sr 4
 2.0
 *Lecture4> sr 5
 2.23606797749979

Review question How can you test this square root program?

To see a bit clearer what goes on here, use the following function:

> iterateFix :: Eq a => (a -> a) -> a -> [a]
> iterateFix f = apprx . iterate f where
>   apprx (x:y:zs) = if x == y then [x] else x: apprx (y:zs)

 *Lecture4> iterateFix (bab 2) 1
 [1.0,1.5,1.4166666666666665,1.4142156862745097,1.4142135623746899,1.414213562373095]
 *Lecture4> iterateFix (bab 3) 1
 [1.0,2.0,1.75,1.7321428571428572,1.7320508100147274,1.7320508075688772]
 *Lecture4> iterateFix (bab 4) 1
 [1.0,2.5,2.05,2.000609756097561,2.0000000929222947,2.000000000000002,2.0]

Fix

Haskell has a special fixpoint operator that can be used to implement recursion.

> fix :: (a -> a) -> a
> fix f = f (fix f)

Alternative definition:

fix f = let x = f x in x

> fbx n = (0,1,n) $$ 
>          fix (\ f (x,y,k) -> if k == 0 then x else f (y,x+y,k-1))

Without using fix:

> fbb n = fbbb (0,1,n) where 
>   fbbb (x,y,n) = if n == 0 then x else fbbb (y,x+y,n-1)

Or in curried notation:

> fbc n = fbbc 0 1 n where 
>   fbbc x y n = if n == 0 then x else fbbc y (x+y) (n-1)

Fix can be used to implement recursion without using other functions that call themselves.

To see why this works, notice that fbx will compute a fixed point of the function

  k = (\ f (x,y,k) ->
        if k == 0 then (x,y,k) else f (y,x+y,k-1))

that is, a function h such that h = k  h:

  h = k h =
     (\ f (x,y,k) ->
        if k == 0 then (x,y,k) else f (y,x+y,k-1)) h
    = \ (x,y,k) ->
        if k == 0 then (x,y,k) else h (y,x+y,k-1))

This is indeed the recursive Fibonacci function.

Review question

Explain the difference between fp and fix.

To see a connection between fp and fix, consider the following definition of fp in terms of fix:

> fp' :: Eq a => (a -> a) -> a -> a
> fp' f = fix 
>          (\ g x -> if x == f x then x else g (f x))

And here are definitions of until and while in terms of fix:

> until' :: (a -> Bool) -> (a -> a) -> a -> a
> until' p f = fix 
>               (\ g x -> if p x then x else g (f x))

> while' :: (a -> Bool) -> (a -> a) -> a -> a
> while' p f = fix 
>               (\ g x -> if not (p x) then x else g (f x))

Let's Talk about QuickCheck

QuickCheck is a very influential random test generation approach to testing.

"In QuickCheck the programmer writes assertions about logical properties that a function should fulfill. Then QuickCheck attempts to generate test cases that falsify these assertions. The project was started in 1999. Besides being used to test regular programs, QuickCheck is also useful for building up a functional specification, for documenting what functions should be doing, and for testing compiler implementations."

The latest version of the code can be found at github. There is a (slightly outdated) QuickCheck Manual. The original paper on QuickCheck is (Claessen and Hughes 2000). This is recommended reading.
QuickCheck Example: The Pebble Game

> data Color = W | B deriving (Eq,Show)
> 
> drawPebble :: [Color] -> [Color]
> drawPebble [] = []
> drawPebble [x] = [x]
> drawPebble (W:W:xs) = drawPebble (B:xs) 
> drawPebble (B:B:xs) = drawPebble (B:xs) 
> drawPebble (W:B:xs) = drawPebble (W:xs) 
> drawPebble (B:W:xs) = drawPebble (W:xs) 

What is the colour of the last pebble?

 *Lecture4> drawPebble [W,W,B,B]
 [B]
 *Lecture4> drawPebble [W,W,B,B,W,W,W]
 [W]
 *Lecture4> drawPebble [W,W,B,B,W,W,W,B,W,B,B]
 [B]
 *Lecture4> drawPebble [W,W,B,B,W,W,W,B,W,B,B,W]
 [W]
 *Lecture4> drawPebble [W,W,B,B,W,W,W,B,W,B,B,W,W,W,W]
 [B]

Test Generators

 newtype Gen a = MkGen{ unGen :: StdGen -> Int -> a }
 
 instance Functor Gen where
   fmap f (MkGen h) =
     MkGen (\r n -> f (h r n))
 
 instance Monad Gen where
   return x =
     MkGen (\_ _ -> x)
   
   MkGen m >>= k =
     MkGen (\r n ->
       let (r1,r2)  = split r
           MkGen m' = k (m r1 n)
        in m' r2 n
     )

RandomGen, next, split

The class RandomGen, defined in System.Random, provides a common interface to random number generators.

Minimal complete definition: next and split.

 next :: g -> (Int, g)

The next operation returns an Int that is uniformly distributed in the range returned by genRange (including both end points), and a new generator.

 split :: g -> (g, g)

The split operation allows one to obtain two distinct random number generators. This is very useful in functional programs (for example, when passing a random number generator down to recursive calls).

StdGen

Instance of RandomGen.

Defined in System.Random module.

choose

Defined in QuickCheck module.

Generates a random element in the given inclusive range.

 choose :: Random a => (a,a) -> Gen a
 choose rng = MkGen (\r _ -> 
              let (x,_) = randomR rng r in x)

Random a

Defined in QuickCheck module.

With a source of random number supply in hand, the Random class allows the programmer to extract random values of a variety of types.

Minimal complete definition: randomR and random.

 randomR :: RandomGen g => (a, a) -> g -> (a, g)

Takes a range (lo,hi) and a random number generator g, and returns a random value uniformly distributed in the closed interval [lo,hi], together with a new generator. It is unspecified what happens if lo>hi. For continuous types there is no requirement that the values lo and hi are ever produced, but they may be, depending on the implementation and the interval.

 random :: RandomGen g => g -> (a, g)

The same as randomR, but using a default range determined by the type:

    For bounded types (instances of Bounded, such as Char), the range is normally the whole type.

    For fractional types, the range is normally the semi-closed interval [0,1).

    For Integer, the range is (arbitrarily) the range of Int.

Class Arbitrary

Defined in QuickCheck module.

Random generation and shrinking of values.

 class Arbitrary a where
   -- | A generator for values of the given type.
   arbitrary :: Gen a
   arbitrary = error "no default generator"
   
   -- | Produces a (possibly) empty list of 
   -- all the possible
   -- immediate shrinks of the given value.
   shrink :: a -> [a]
   shrink _ = []
 

Making Color an Instance of Arbitrary

> instance Arbitrary Color where
>    arbitrary = oneof [return W, return B]

This allows QuickCheck to derive Arbitrary [Color] ...

Here is how:

 instance Arbitrary a => Arbitrary [a] where
   arbitrary = sized $ \n ->
     do k <- choose (0,n)
        sequence [ arbitrary | _ <- [1..k] ]
 
   shrink xs = removeChunks xs ...

sample for generating example values

 sample' :: Gen a -> IO [a]
 sample' (MkGen m) =
   do rnd0 <- newStdGen
      let rnds rnd = rnd1 : rnds rnd2 
                     where (rnd1,rnd2) = split rnd
      return [(m r n) | 
              (r,n) <- rnds rnd0 `zip` [0,2..20] ]
 

Generating some example values and print them to stdout:

 sample :: Show a => Gen a -> IO ()
 sample g =
   do cases <- sample' g
      sequence_ (map print cases)

Example Use

 *Lecture4> sample $ (arbitrary :: Gen [Color])
 []
 [W]
 [W,W]
 [W,B,W]
 [W,B,W,B]
 [B,B,B,W,B,W,W]
 [B,W,B,W,B,W,B,W,B,B,W]
 []
 [B,B,B,B,B,W,B,B,W,B,W,W,W,B]
 [W,W,B,B,W,W,B,W,B,B,B,B,W,W,B]
 [W,W,W,W,W,W,B,B,B,W,B,W,B,B]
 

Stating an Invariant

> numberW :: [Color] -> Int
> numberW = length . (filter (== W)) 
>     
> parityW :: [Color] -> Int
> parityW xs =  mod (numberW xs) 2
>     
> prop_invariant xs = 
>   parityW xs == parityW (drawPebble xs)

Testing This ...

 Lecture4> quickCheck prop_invariant
 +++ OK, passed 100 tests.
  

See what happens with an unreasonable property

Strange invariant:

> prop_length xs = length xs == length (drawPebble xs)

 Lecture4> quickCheck prop_length
 *** Failed! Falsifiable (after 7 tests and 1 shrink):     
 [B,B]
 *Lecture4> quickCheck prop_length
 *** Failed! Falsifiable (after 6 tests and 1 shrink):     
 [B,W]
 *Lecture4> quickCheck prop_length
 *** Failed! Falsifiable (after 3 tests):                  
 [B,B]
 

Workshop this week: Workshop4.pdf.

Lab work this week: Lab4.html.

Back to main course page.

Claessen, Koen, and John Hughes. 2000. “QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs.” In Proc. of International Conference on Functional Programming (ICFP). ACM SIGPLAN.
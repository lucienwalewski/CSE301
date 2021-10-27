module Formative3 where

import Data.Maybe
import Data.List

import System.Random hiding (uniform)

-- Exercise 1

{-
fun1 :: a -> [a]
fun1 x = [x]

fun2 :: ([a] -> b) -> b
fun2 x = x []

fun3 :: (a -> b -> c) -> b -> a -> c
fun3 x y z = x z y

fun4 :: (a -> a) -> a -> a
fun4 x y = x (x (x (x y)))

fun5 x y = x y x
This function does not exist. From the input we see that
x is a function that takes y as input. However, in the output,
x has no input.

Ghci raises an error on infinite type
-}

-- Exercise 2

{-
mysteryfun1 :: (a -> b -> c) -> (b -> a -> c)

mysteryfun2 :: (a -> a -> b) -> (a -> a -> b)
-}

{- mysteryfun1 takes a function
of type (a -> b -> c) and produces a function of type 
(b -> a -> c). Therefore, it is probable that this new function
may apply some operation to both its arguments of type a and b 
respectively, preserving their types before applying the input function 
with the two inputs swapped.

mysteryfun2  takes a function taking two inputs of the same types returning 
a new type and produces a function with the same input and output types. Again, 
the new function may just be modifying the first and second arguments, preserving
their types before applying the original input function.
-}

-- Exercise 3

-- Q3.1

newtype Down a = Down a  deriving (Show,Eq)

instance Ord a => Ord (Down a) where
   compare (Down a) (Down b) = compare b a
-- ...

-- Q3.2

data BT a = Empty | Fork a (BT a) (BT a)  deriving (Show)

instance Eq a => Eq (BT a) where
   (==) Empty Empty = True
   (==) Empty _ = False
   (==) _ Empty = False
   (==) (Fork a1 l1 r1) (Fork a2 l2 r2) = (a1 == a2) && (l1 == l2) && (r1 == r2)

instance Ord a => Ord (BT a) where
   compare Empty Empty = EQ
   compare Empty _ = LT
   compare _ Empty = GT
   compare (Fork a1 l1 r1) (Fork a2 l2 r2) = case compare a1 a2 of
                                                  LT -> LT
                                                  GT -> GT
                                                  EQ -> case compare l1 l2 of
                                                     EQ -> compare r1 r2
                                                     LT -> LT
                                                     GT -> GT





-- Q3.3

class Mon m where
  monop :: m -> m -> m  
  monid :: m            

instance Mon [a] where
  monop = (++)
  monid = []

instance Mon a => Mon (b -> a) where
  monop fun1 fun2 = \x -> monop (fun1 x) (fun2 x)
  monid = const monid

newtype MonOpp a = MonOpp a   deriving (Show,Eq)

instance Mon a => Mon (MonOpp a) where
  monop x y = monop y x
  monid = monid

-- Monoids satisfy the properties of associativity and the neutral element.

-- Exercise 4

-- If you've made it through the other exercises and the suggested
-- reading, you can have a go at implementing randomized quicksort below!
-- You only need to complete the rqsort function.

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (p:xs) = qsort [x | x <- xs, x < p]
                ++ [p]
                ++ qsort [x | x <- xs, x >= p]

data Rand a = Generator (StdGen -> (a , StdGen))

instance Monad Rand where
  return x = Generator (\g -> (x,g))
  Generator h >>= f = Generator (\g -> let (x, g') = h g
                                           (Generator h') = f x
                                       in h' g')

instance Functor Rand where
   fmap f xm = xm >>= return . f

instance Applicative Rand where
   pure = return
   fm <*> xm = fm >>= \f -> xm >>= return . f

runRand :: Int -> Rand a -> a
runRand seed (Generator h) = fst (h (mkStdGen seed))

randInt :: Rand Int
randInt = Generator random

randIntR :: (Int, Int) -> Rand Int
randIntR (lower, upper) = Generator (randomR (lower, upper))

uniform :: [a] -> Rand a
uniform [] = undefined
uniform xs = do
              n <- randIntR (0, length xs - 1)
              return(xs !! n)

getPivot :: [a] -> Int -> (a, [a])
getPivot (x:xs) 0 = (x,xs)
getPivot (x:xs) n = let (p,ys) = getPivot xs (n-1) in (p, x:ys)

rqsort :: Ord a => [a] -> Rand [a]
rqsort xs = undefined

rqsort' :: Ord a => [a] -> [a]
rqsort' xs = runRand seed (rqsort xs)
             where seed = 42 -- say

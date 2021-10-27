{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad.State
import Data.List
import Data.Maybe
import Foreign.C (e2BIG)
import GHCi (EvalResult (EvalException))
import Llvm (LlvmCallConvention (CC_X86_Stdcc), LlvmStatement (Expr, Store))
import Llvm.AbsSyn (LlvmStatement (Store))
import System.Random

class Monad m => SelectMonad m where
  select :: [a] -> m a

instance SelectMonad [] where
  select xs = xs

instance SelectMonad IO where
  select xs
    | not (null xs) = do
      i <- getStdRandom (randomR (0, length xs -1))
      return (xs !! i)
    | otherwise = error ("cannot select from empty list")

newtype Dist a = Dist {dist :: [(a, Rational)]} deriving (Show)

instance Monad Dist where
  return x = Dist [(x, 1)]
  xm >>= f = Dist [(y, p * q) | (x, p) <- dist xm, (y, q) <- dist (f x)]

instance Functor Dist where
  fmap f xm = xm >>= return . f

instance Applicative Dist where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

instance SelectMonad Dist where
  select xs
    | not (null xs) = let n = length xs in Dist [(x, 1 / fromIntegral n) | x <- xs]
    | otherwise = error ("cannot select from empty list")

code :: SelectMonad m => m Char
code = do
  i <- select [0 .. 3]
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y, p) <- dist xm, x == y]

normalize :: Eq a => Dist a -> Dist a
normalize xm = Dist [(x, prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x, p) <- dist xm, p > 0] -- "nub", defined in Data.List, removes duplicates from a list

choose :: SelectMonad m => Int -> [a] -> m [a]
choose k xs
  | k == 0 = return []
  | otherwise = do
    i <- select [0 .. length xs - 1]
    let (ys, x : zs) = splitAt i xs
    us <- choose (k -1) (ys ++ zs)
    return (x : us)

simulate :: Monad m => Int -> m Bool -> m Int
simulate 0 bm = return 0
simulate n bm = do
  k <- simulate (n -1) bm
  b <- bm
  if b then return (k + 1) else return k

data Bin a = L a | B (Bin a) (Bin a) deriving (Show, Eq)

genTree :: SelectMonad m => [a] -> m (Bin a)
genTree = undefined

type Var = String

data Expr
  = Num Int
  | Plus Expr Expr
  | Times Expr Expr
  | Minus Expr Expr
  | Div Expr Expr
  | Var Var
  deriving (Show, Read)

data Cmd = Eval Expr | Asn Var Expr
  deriving (Show, Read)

type Store = Var -> Maybe Int

eval1 :: Expr -> State Store Int
eval1 (Num n) = return n
eval1 (Plus e1 e2) = do
  n1 <- eval1 e1
  n2 <- eval1 e2
  return (n1 + n2)
eval1 (Times e1 e2) = do
  n1 <- eval1 e1
  n2 <- eval1 e2
  return (n1 * n2)
eval1 (Minus e1 e2) = do
  n1 <- eval1 e1
  n2 <- eval1 e2
  return (n1 - n2)
eval1 (Div e1 e2) = do
  n1 <- eval1 e1
  n2 <- eval1 e2
  if n2 == 0 then error "Divide by zero error" else return (n1 `div` n2)
eval1 (Var n) = do
  store <- get
  case store n of
    Nothing -> error "Undefined variable"
    Just n -> return n

run1 :: Cmd -> State Store (Maybe Int)
run1 (Eval expr) = do
  x <- eval1 expr
  return (Just x)
run1 (Asn v expr) = do
  store <- get
  e <- eval1 expr
  put (update (v, e) store)
  return Nothing

update :: (Var, Int) -> Store -> Store
update (x, v) rho y = if y == x then Just v else rho y

calc1 :: IO ()
calc1 = go (const Nothing)
  where
    go :: Store -> IO ()
    go rho = do
      s <- getLine
      let c = read s
      case runState (run1 c) rho of
        (Nothing, rho') -> go rho'
        (Just v, rho') -> putStrLn (show v) >> go rho'

eval2 :: Expr -> StateT Store (Either String) Int
eval2 (Num n) = return n
eval2 (Plus e1 e2) = do
  n1 <- eval2 e1
  n2 <- eval2 e2
  return (n1 + n2)
eval2 (Times e1 e2) = do
  n1 <- eval2 e1
  n2 <- eval2 e2
  return (n1 * n2)
eval2 (Minus e1 e2) = do
  n1 <- eval2 e1
  n2 <- eval2 e2
  return (n1 - n2)
eval2 (Div e1 e2) = do
  n1 <- eval2 e1
  n2 <- eval2 e2
  if n2 == 0 then abort "Division by zero error" else return (n1 `div` n2)
eval2 (Var n) = do
  store <- get
  case store n of
    Nothing -> abort "Undeclared variable"
    Just n -> return n

abort :: [Char] -> StateT Store (Either String) Int
abort err = lift (Left err)

run2 :: Cmd -> StateT Store (Either String) (Maybe Int)
run2 (Eval expr) = do
  x <- eval2 expr
  return (Just x)
run2 (Asn v expr) = do
  store <- get
  e <- eval2 expr
  put (update (v, e) store)
  return Nothing

calc2 :: IO ()
calc2 = go (\_ -> Nothing)
  where
    go :: Store -> IO ()
    go rho = do
      s <- getLine
      let c = read s
      case runStateT (run2 c) rho of
        Left err -> putStrLn ("error: " ++ err) >> go rho
        Right (Nothing, rho') -> go rho'
        Right (Just v, rho') -> putStrLn (show v) >> go rho'

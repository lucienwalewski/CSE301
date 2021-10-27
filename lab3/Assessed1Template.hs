import Data.Maybe

-- Exercise 1

-- Q1.1
my_or :: [Bool] -> Bool
my_or = or

-- Q1.2
my_takeWhile :: (a -> Bool) -> [a] -> [a]
my_takeWhile p = foldr (\x xs -> if p x then x : xs else []) []

-- Q1.3
my_intersperse :: a -> [a] -> [a]
my_intersperse v = foldr (\x xs -> if not (null xs) then x : v : xs else x : xs) []

-- Q1.4
my_tails :: [a] -> [[a]]
my_tails = foldr (\x xs -> (x : head xs) : xs) [[]]

-- Q1.5
-- my_isPrefixOf :: Eq a => [a] -> [a] -> Bool
-- my_isPrefixOf = foldr (\x xs -> ) True

-- Exercise 2

data Tree a b = Leaf a | Node b [Tree a b] deriving (Show, Eq)

texample :: Tree Char Integer
texample = Node 1 [Node 2 [Leaf 'a', Leaf 'b'], Node 3 [Leaf 'c', Leaf 'd', Leaf 'e'], Node 4 []]

bst :: Tree () Char
bst = Node 'c' [Node 'a' [Leaf (), Node 'b' [Leaf (), Leaf ()]], Node 'd' [Leaf (), Leaf ()]]

foldTree :: (a -> c) -> (b -> [c] -> c) -> Tree a b -> c
foldTree f g (Leaf a) = f a
foldTree f g (Node b ts) = g b (map (foldTree f g) ts)

-- Q2.1
leaves :: Tree a b -> Int
leaves = foldTree (const 1) (\b is -> 0 + sum is)

-- Q2.2
nodes :: Tree a b -> Int
nodes = foldTree (const 0) (\b is -> 1 + sum is)

-- Q2.3
preorder :: Tree a b -> [Either a b]
preorder = foldTree (\a -> [Left a]) (\b y -> Right b : concat y)

-- Exercise 3

-- (Write your proofs as comments below.)
{-
   You can use Haskell's syntax for multiline comments
   if you wish to spread your comments across multiple
   lines without prefixing them by "--"s.
-}

-- Q3.1

{-
  We show that reverse (xs ++ ys) = reverse ys ++ reverse xs for
  finite xs and ys.
  Let us proceed by induction on xs.
    Base case: xs = []
      Then it follows that

        reverse (xs ++ ys) = reverse ([] ++ ys)
                           = reverse ys
                           = reverse ys ++ []
                           = reverse ys ++ reverse xs

      and we are done.
    Inductive step:
      Assume that the statement holds for lists xs of size n.
      Then for lists of size n+1, we can decompose xs as

        xs = x:xs'

      where xs' is a list of size n. Then,

        reverse (xs ++ ys) = reverse (x:xs' ++ ys)
                           = reverse (x:(xs' ++ ys))
                           = reverse (xs' ++ ys) ++ [x]

      By induction, reverse(xs' ++ ys) = reverse ys ++ reverse xs'.
      It follows that

        reverse (xs ++ ys) = reverse ys ++ reverse xs' ++ [x]
                           = reverse ys ++ reverse xs

      and we are done.
-}

-- Q3.2

{-
  We show that revapp xs ys = (reverse xs) ++ ys for finite xs and ys.
  Let us proceed by induction on xs.
    Base case: xs = []

      revapp xs ys = revapp [] ys
                   = ys
                   = (reverse []) ++ ys
                   = (reverse xs) ++ ys

    Inductive step:
      Assume that the statement holds for lists xs of size n.
      Then as before we decompose xs as x:xs'. Then,

        revapp xs ys = revapp x:xs' ys
                     = revapp xs' (x:ys)
                     = (reverse xs') ++ (x:ys)
                     = (reverse xs) ++ ys

      as required.

    Taking ys = [], we have the desired result:

      revverse xs = revapp xs []
-}

-- Exercise 4

type Var = String

data Lit = Lit {var :: Var, pol :: Bool} deriving (Show, Eq)

data Cls = BigOr {literals :: [Lit]} deriving (Show, Eq)

data Frm = BigAnd {clauses :: [Cls]} deriving (Show, Eq)

lem = BigAnd [BigOr [Lit "x" True, Lit "x" False]]

con = BigAnd [BigOr [Lit "x" True], BigOr [Lit "x" False]]

frm =
  BigAnd
    [ BigOr [Lit "x" True, Lit "y" True],
      BigOr [Lit "x" False, Lit "y" False]
    ]

fr' =
  BigAnd
    [ BigOr [Lit "x1" True, Lit "x2" True, Lit "x3" False],
      BigOr [Lit "x2" True, Lit "x3" True, Lit "x4" False],
      BigOr [Lit "x3" True, Lit "x4" True, Lit "x1" True],
      BigOr [Lit "x4" True, Lit "x1" False, Lit "x2" True],
      BigOr [Lit "x1" False, Lit "x2" False, Lit "x3" True],
      BigOr [Lit "x2" False, Lit "x3" False, Lit "x4" True],
      BigOr [Lit "x3" False, Lit "x4" False, Lit "x1" False]
    ]

type Subst = [(Var, Bool)]

lookupVar :: Subst -> Var -> Bool
lookupVar rho x = fromJust $ lookup x rho

-- Q4.1
evalLit :: Subst -> Lit -> Bool
evalLit [] _ = True
evalLit ((v, b) : subs) (Lit var bool) = if v == var then res else evalLit subs (Lit var bool)
  where
    res = if bool then b else not b

-- Q4.2
evalCls :: Subst -> Cls -> Bool
evalCls _ (BigOr []) = False
evalCls s (BigOr (l : lits)) = evalLit s l || evalCls s (BigOr lits)

-- Q4.3
evalFrm :: Subst -> Frm -> Bool
evalFrm _ (BigAnd []) = True
evalFrm s (BigAnd (c : cls)) = evalCls s c && evalFrm s (BigAnd cls)

-- Q4.4

varsFrm :: Frm -> [String]
varsFrm frm = varsFrmAcc frm []
  where
    varsFrmAcc :: Frm -> [String] -> [String]
    varsFrmAcc (BigAnd []) vars = vars
    varsFrmAcc (BigAnd (c : cls)) vars = varsFrmAcc (BigAnd cls) (varsClsAcc c vars)
      where
        varsClsAcc :: Cls -> [String] -> [String]
        varsClsAcc (BigOr []) vars = vars
        varsClsAcc (BigOr ((Lit v b) : ls)) vars = if v `elem` vars then varsClsAcc (BigOr ls) vars else varsClsAcc (BigOr ls) (v : vars)

-- Q4.5
allSubsts :: Frm -> [Subst]
allSubsts frm = generSubsts (varsFrm frm) []
  where
    generSubsts :: [String] -> [Subst] -> [Subst]
    generSubsts [] substs = substs
    generSubsts (v : vars) [] = generSubsts vars [[(v, True)], [(v, False)]]
    generSubsts (v : vars) substs = generSubsts vars (addVar (v, True) substs) ++ generSubsts vars (addVar (v, False) substs)
      where
        addVar :: (Var, Bool) -> [Subst] -> [Subst]
        addVar (v, b) [] = []
        addVar (v, b) (s : substs) = ((v, b) : s) : addVar (v, b) substs

-- Q4.6
sat :: Frm -> Bool
sat frm = foldr (\s substs -> evalFrm s frm || substs) False (allSubsts frm)

-- Q4.7
taut :: Frm -> Bool
taut frm = foldr (\s substs -> evalFrm s frm && substs) True (allSubsts frm)

-- Q4.8
solutions :: Frm -> [Subst]
solutions frm = foldr (\s substs -> if evalFrm s frm then s : substs else substs) [] (allSubsts frm)

kcolor :: Int -> ([Int], [(Int, Int)]) -> Frm
kcolor k (vs, es) =
  BigAnd $
    [BigOr [Lit (hascol v c) True | c <- [1 .. k]] | v <- vs]
      ++ [BigOr [Lit (hascol v c) False, Lit (hascol u c) False] | (v, u) <- es, c <- [1 .. k]] -- every vertex has some color, and
      -- no two neighbors have the same color
  where
    hascol :: Int -> Int -> Var
    hascol v c = "hc" ++ show (v, c)

prismGraph :: ([Int], [(Int, Int)])
prismGraph = ([1 .. 6], [(1, 2), (1, 3), (1, 4), (2, 3), (2, 6), (3, 5), (4, 5), (4, 6), (5, 6)])

completeGraph n = ([1 .. n], [(i, j) | i <- [1 .. n], j <- [1 .. n], i /= j])

varF :: Var -> Frm
varF x = BigAnd [BigOr [Lit x True]]

ttF :: Frm
ttF = BigAnd []

ffF :: Frm
ffF = BigAnd [BigOr []]

-- Q4.9
andF :: Frm -> Frm -> Frm
andF (BigAnd frm1) (BigAnd frm2) = BigAnd (frm1 ++ frm2)

-- Q4.10
orF :: Frm -> Frm -> Frm
-- orF (BigAnd []) (BigAnd cls') = BigAnd []
orF (BigAnd []) (BigAnd cls') = BigAnd []
orF (BigAnd cls) (BigAnd []) = BigAnd cls
orF (BigAnd (c : cls)) (BigAnd cls') = andF (generAnds c (BigAnd cls')) (orF (BigAnd cls) (BigAnd cls'))
  where
    generAnds :: Cls -> Frm -> Frm
    generAnds _ (BigAnd []) = BigAnd []
    generAnds (BigOr c) (BigAnd ((BigOr c') : cls')) = andF (BigAnd [BigOr (c ++ c')]) (generAnds (BigOr c) (BigAnd cls'))

-- Q4.11
notC :: Cls -> Frm
notC (BigOr []) = BigAnd []
notC (BigOr ((Lit v b) : cls)) = andF (BigAnd [BigOr [Lit v (not b)]]) (notC (BigOr cls))

notF :: Frm -> Frm
notF (BigAnd []) = BigAnd []
notF (BigAnd (c : cls)) = orF (notC c) (notF (BigAnd cls))


impF :: Frm -> Frm -> Frm
impF p q = notF p `orF` q

iffF :: Frm -> Frm -> Frm
iffF p q = (p `impF` q) `andF` (q `impF` p)

peirce :: Frm
peirce = ((p `impF` q) `impF` p) `impF` p
  where
    p = varF "p"
    q = varF "q"

-- Q4.12
-- (Test your SAT solver and write your observations as a comment below.)

-- Q4.13
taut' :: Frm -> Bool
taut' = not . sat . notF

-- (Write your observations about taut' vs taut as a comment below.)

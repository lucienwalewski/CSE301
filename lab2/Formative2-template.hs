import Data.List
import Data.Maybe
import Distribution.Simple.Utils (xargs)

-- Exercise 1

doubleList :: [a] -> [a]
doubleList [] = []
doubleList (x : xs) = [x, x] ++ doubleList xs

firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled [] = Nothing
firstDoubled [x] = Nothing
firstDoubled (x : y : xs) = if x == y then Just x else firstDoubled (y : xs)

-- Exercise 2

data Allergen = Nuts | Gluten | Soy | Dairy deriving (Show, Eq)

type Recipe = [Allergen]

data Price = P Int deriving (Show, Eq, Ord)

data Cupcake = CC Price Recipe deriving (Show, Eq)

priceInRange :: Price -> Price -> Cupcake -> Bool
priceInRange minPrice maxPrice (CC p r) = if minPrice <= p && p <= maxPrice then True else False

priceRange :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange _ _ [] = []
priceRange minPrice maxPrice (cc : ccs) =
  if priceInRange minPrice maxPrice cc
    then [cc] ++ priceRange minPrice maxPrice ccs
    else priceRange minPrice maxPrice ccs

isInAs :: [Allergen] -> Allergen -> Bool
isInAs as a = if elem a as then True else False

containsAllergens :: [Allergen] -> Cupcake -> Bool
containsAllergens _ (CC _ []) = False
containsAllergens as (CC p (all : alls)) =
  if isInAs as all
    then True
    else containsAllergens as (CC p alls)

allergyFree :: [Allergen] -> [Cupcake] -> [Cupcake]
allergyFree _ [] = []
allergyFree as (cc : ccs) =
  if containsAllergens as cc
    then allergyFree as ccs
    else [cc] ++ allergyFree as ccs

-- Exercise 3

type Tin = [Recipe]

data Spec = And Spec Spec | Or Spec Spec | Not Spec | HasCup Int Allergen deriving (Show, Eq)

sampletin :: Tin
sampletin = [[Nuts], [Dairy, Gluten], [], [Soy]]

checkSpec :: Spec -> Tin -> Bool
checkSpec (And s1 s2) tin = (checkSpec s1 tin) && (checkSpec s2 tin)
checkSpec (Or s1 s2) tin = (checkSpec s1 tin) || (checkSpec s2 tin)
checkSpec (Not s) tin = not (checkSpec s tin)
checkSpec (HasCup k x) tin = elem x (tin !! k)

-- Exercise 4

data Tree a b = Leaf a | Node b [Tree a b] deriving (Show, Eq)

texample :: Tree Char Integer
texample = Node 1 [Node 2 [Leaf 'a', Leaf 'b'], Node 3 [Leaf 'c', Leaf 'd', Leaf 'e'], Node 4 []]

bst :: Tree () Char
bst = Node 'c' [Node 'a' [Leaf (), Node 'b' [Leaf (), Leaf ()]], Node 'd' [Leaf (), Leaf ()]]

preorders :: [Tree a b] -> [Either a b]
preorders [] = []
preorders [a] = preorder a
preorders (t : trees) = preorder t ++ preorders trees

preorder :: Tree a b -> [Either a b]
preorder (Leaf a) = [Left a]
preorder (Node a tree) = Right a : preorders tree

-- Exercise 5

linearSortAux :: Ord a => [a] -> [a] -> [a]
linearSortAux [] x = x
linearSortAux (x : xs) [] = linearSortAux xs [x]
linearSortAux (x : xs) (s : ss) =
  if x > s
    then s : linearSortAux (x : xs) ss
    else linearSortAux xs (x : s : ss)

linearSort :: Ord a => [a] -> [a]
linearSort a = linearSortAux a []

-- Exercise 6

counterexample :: [Int]
counterexample = undefined

data Bin = L | B Bin Bin deriving (Show, Eq)

fromBin :: Bin -> [Int]
fromBin = undefined

toBin :: [Int] -> Maybe Bin
toBin = undefined

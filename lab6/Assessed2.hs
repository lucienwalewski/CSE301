-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Assessed2 where

import Data.List
import Data.Tree
import DomViz
import System.Random
import Types

{-
  Uncomment the previous line if you want to use the visualization
  routines. You will need to have the diagrams library, which you
  should be able to install by running:

  $ cabal install --lib diagrams diagrams-contrib diagrams-lib diagrams-svg

-}

-- given a cell c and a player p, compute the adjacent cell c'
-- that is also occupied if p plays a domino at c
adjCell :: Cell -> Player -> Cell
adjCell (x, y) H = (x + 1, y)
adjCell (x, y) V = (x, y + 1)

-- compute the opponent of a player
opp :: Player -> Player
opp H = V
opp V = H

-- determine whether a move is valid in a given board
valid :: Board -> Cell -> Bool
valid b c = c `elem` free b && adjCell c (turn b) `elem` free b

-- create an empty board from an arbitrary list of cells
empty :: [Cell] -> Board
empty cs = Board {turn = H, free = cs, hist = []}

-- create a rectangular board of arbitrary dimensions
board :: Int -> Int -> Board
board maxx maxy = empty [(x, y) | x <- [1 .. maxx], y <- [1 .. maxy]]

-- create a crosshatch-shaped square board of arbitrary dimension
hatch :: Int -> Board
hatch n = empty [(x, y) | x <- [1 .. 2 * n + 1], y <- [1 .. 2 * n + 1], odd y || x == 1 || x == (2 * n + 1) || odd x]

-- some example Domineering games
board4x4_3 =
  Board
    { turn = H,
      free = [(1, 1), (1, 2), (2, 2), (2, 3), (2, 4), (3, 2), (3, 3), (3, 4), (4, 1), (4, 2), (4, 3), (4, 4)],
      hist = [(1, 3), (2, 1)]
    }

alphaDom_vs_LeeSedom =
  Board
    { turn = V,
      free = [(-4, 1), (-4, 3), (-2, 0), (-2, 4), (2, 1), (2, 4), (3, -4), (3, 4), (4, -2), (4, 0)],
      hist = [(0, 4), (4, 1), (0, -4), (-4, -3), (-1, -2), (2, -1), (-2, -4), (-4, -1), (-1, 2), (4, 3), (1, 2), (-2, 2), (-4, -4), (-2, -2), (2, -2), (4, -4), (-3, 1), (2, -4), (-4, 4), (-1, 3), (-4, 2), (-3, -2), (3, -1), (1, -3), (-2, -3), (3, 1), (1, 3)]
    }

alphaDom_vs_RanDom =
  Board
    { turn = V,
      free = [(-4, -3), (-4, 0), (-2, -4), (-2, -2), (-1, -4), (-1, -2), (-1, 2), (-1, 4), (0, -4), (0, -2), (0, 2), (0, 4), (1, -4), (1, -2), (1, 2), (1, 4), (2, -4), (2, -2), (2, 4), (3, -4), (4, 0), (4, 3)],
      hist = [(-3, 4), (2, -1), (-3, 2), (4, -2), (-4, -4), (-4, 3), (3, 4), (2, 1), (-3, 1), (3, 1), (-4, -1), (-2, -1), (-2, 3), (-4, 1), (1, 3), (4, -4), (-4, -2), (4, 1), (1, -3), (3, -2), (-2, -3)]
    }

-- start of Exercise 1

-- Q1.1
legalMoves :: Player -> Board -> [Cell]
legalMoves player board@Board {free = cells} = [move | move <- cells, valid board {turn = player} move]

-- Q1.2
moveLegal :: Board -> Cell -> Board
moveLegal board@Board {turn = player, free = free_cells, hist = hist_cells} move = Board {turn = next_player, free = new_free, hist = new_hist}
  where
    next_player = opp player
    new_free = delete (adjCell move player) (delete move free_cells)
    new_hist = move : hist_cells

-- Q1.3
replay :: Board -> [Board]
replay board@Board {hist = []} = [board]
replay board@Board {turn = player, free = free_cells, hist = last_move : hist_cells} = replay previous_board ++ [board]
  where
    opponent = opp player
    previous_board = board {turn = opponent, free = free_cells ++ [last_move, adjCell last_move opponent], hist = hist_cells}

-- start of Exercise 2

gametree :: Board -> Tree Board
gametree b = Node b [gametree (moveLegal b c) | c <- legalMoves (turn b) b]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

-- Q2.1
score :: Board -> Score
score board@Board {turn = player, free = free_cells, hist = hist_cells} = if null (legalMoves player board) then Win (opp player) else Heu x
  where
    sign = if player == H then -1 else 1
    x = length (legalMoves V board) - length (legalMoves H board) - sign

-- Q2.2
minimax :: (Board -> Score) -> Tree Board -> Tree (Board, Score)
minimax score (Node board []) = Node (board, score board) []
minimax score (Node board@Board {turn = player} trees) =
  if player == H
    then Node (board, minimum scores) subtrees
    else Node (board, maximum scores) subtrees
  where
    subtrees = [minimax score c | c <- trees]
    scores = [score | Node (_, score) _ <- subtrees]

-- Q2.3
bestmoves :: Int -> (Board -> Score) -> Board -> [Cell]
bestmoves depth score board = [head (hist moves) | moves <- equivalent_heuristic]
  where
    equivalent_heuristic = [moves | Node (moves, score) _ <- subtrees, score == best]
    tree = prune depth (gametree board)
    Node (_, best) subtrees = minimax score tree

-- start of Exercise 3

class Monad m => SelectMonad m where
  select :: [a] -> m a

instance SelectMonad [] where
  select xs = xs

instance SelectMonad IO where
  select xs
    | not (null xs) = do
      i <- getStdRandom (randomR (0, length xs -1))
      return (xs !! i)
    | otherwise = error "cannot select from empty list"

selectSafe :: SelectMonad m => [a] -> m (Maybe a)
selectSafe [] = return Nothing
selectSafe xs = select xs >>= \x -> return (Just x)

randomBestPlay :: SelectMonad m => Int -> (Board -> Score) -> Board -> m (Maybe Cell)
randomBestPlay d sfn = selectSafe . bestmoves d sfn

randomPlay :: SelectMonad m => Board -> m (Maybe Cell)
randomPlay b = selectSafe (legalMoves (turn b) b)

-- Q3.1
runGame :: SelectMonad m => (Board -> m (Maybe Cell)) -> (Board -> m (Maybe Cell)) -> Board -> m Board
runGame playH playV board = do
  play_cell <- if turn board == H then playH board else playV board
  case play_cell of
    Nothing -> return board
    Just c -> do
      if (turn board == H && c `elem` legalMoves H board) || (turn board == V && c `elem` legalMoves V board)
        then do
          runGame playH playV (moveLegal board c)
        else return board

-- Q3.2
generateCarpet :: Int -> Board -> [Board]
generateCarpet n b = new_b : generateCarpet (3 * n) new_b
  where
    new_turn = opp (turn b)
    new_free =
      free b ++ [(x + n, y) | (x, y) <- free b]
        ++ [(x + 2 * n, y) | (x, y) <- free b]
        ++ [(x, y + n) | (x, y) <- free b]
        ++ [(x, y + 2 * n) | (x, y) <- free b]
        ++ [(x + n, y + 2 * n) | (x, y) <- free b]
        ++ [(x + 2 * n, y + n) | (x, y) <- free b]
        ++ [(x + 2 * n, y + 2 * n) | (x, y) <- free b]
    new_b = b {free = new_free, turn = new_turn}

initialBoard = Board {turn = H, free = [(1, 1)], hist = []}

carpets :: [Board]
carpets = initialBoard : generateCarpet 1 initialBoard
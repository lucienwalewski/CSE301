merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (xs) = merge (msort ys) (msort zs)
                  where
                    n = (length xs) `div` 2
                    ys = take n xs
                    zs = drop n xs

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = (x <= y) && isSorted (y:xs)

main :: IO()
main = interact(unlines . msort . lines)

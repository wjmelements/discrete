module Sets
( subset
, subsets
, power_set
) where

import Numbers

subsets :: [a] -> [[a]]
subsets set = [] : helper [[]] set

helper :: [[a]] -> [a] -> [[a]]
helper done [] = []
helper current next = let added = map ((head next):) current
			in added ++ helper (current ++ added) (tail next)

subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True
subset _ [] = False
subset part whole = elem (head part) whole && subset (tail part) whole

power_set :: [a] -> [[a]]
power_set = subsets

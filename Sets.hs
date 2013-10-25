module Sets
( subset
, subsets
, power_set
, intersect
, difference
, union
, unions
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

intersect :: Eq a => [a] -> [a] -> [a]
intersect one two = [member | member <- two, elem member one]

difference :: Eq a => [a] -> [a] -> [a]
difference one two = [member | member <- one, not $ elem member two]

union :: Eq a => [a] -> [a] -> [a]
union one two = one ++ filter (flip notElem one) two

unions :: Eq a => [[a]] -> [a]
unions = foldr union []

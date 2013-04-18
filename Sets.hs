module Sets
( power_set
) where

import Numbers

power_set :: [a] -> [[a]]
power_set [] = [[]]
power_set set = finite_subsets (tail set) ++ map ((head set):) (finite_subsets (tail set))

module Sets
( power_set
) where

import Numbers

power_set :: [a] -> [[a]]
power_set [] = [[]]
power_set set = power_set (tail set) ++ map ((head set):) (power_set (tail set))

module Numbers
( naturals
, integers
, rationals -- currently only positive
) where

naturals :: [Integer]
naturals = [1..]

integers :: [Integer]
integers = [if even x then negate $ div x 2 else div (x-1) 2 | x <- naturals]

rationals :: [Double]
rationals = [(fromIntegral(a-b))/(fromIntegral(b+1)) | a <- naturals, b <- [0..a], 1 == gcd (a-b) (b+1)]

module Numbers
( naturals
, integers
, rationals
) where

naturals :: [Integer]
naturals = [1..]

integers :: [Integer]
integers = [if even x then negate $ div x 2 else div (x-1) 2 | x <- naturals]

rationals :: [Double]
rationals = 0:
		[
			let c = div b 2 in 
				if even b then fromIntegral(a-c)/fromIntegral(c+1) 
				else negate $ (fromIntegral(a-c)/fromIntegral(c+1))
			| a <- naturals
			, b <- [0..(a*2-1)]
			, let c = div b 2 in 1 == gcd (a-c) (c+1)
		]

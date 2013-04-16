module Numbers.Primes
( divides
, factor
, prime
, primes
, prime_factor
) where

import Numbers

divides :: Integral a => a -> a -> Bool
divides x y = mod x y == 0

stored_primes :: [Integer]
stored_primes = [2]

factor :: Integral a => a -> [a]
factor x
	| x < 0 = -1 : factor (-x)
	| otherwise = filter (divides x) [1..x]

prime :: Integer -> Bool
prime x
	| elem x stored_primes = True
	| otherwise = factor x == [1,x]


primes :: [Integer]
primes = filter prime naturals

prime_factor :: Integer -> [Integer]
prime_factor x
	| elem x [0,1] = []
	| otherwise = 
		let element = head (filter (divides x) primes)
		in element : prime_factor (div x element)


-- Sorting Algorthims

-- Quicksort

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- Mergesort 

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | (x < y) = x: merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys
 
split:: [a] -> ([a], [a])
split xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2 
 
mergesort :: (Ord a) => [a] -> [a]
mergesort xs 
    | (length xs) > 1 = merge (mergesort ls) (mergesort rs)
    | otherwise = xs
    where (ls, rs) = split xs


-- Bubblesort

increment :: (Ord a) => [a] -> [a]
increment (x:y:xs)
   | x > y = y : increment (x:xs)
   | otherwise = x : increment (y:xs)
increment (x) = (x)

bubblesort :: (Ord a) => [a] -> Int -> [a]
bubblesort xs i 
    | i == (length xs) = xs
    | otherwise = bubblesort (increment xs) (i + 1) 
 
bubblesort' :: (Ord a) => [a] -> [a]
bubblesort' xs = bubblesort xs 0

-- Fizz Buzz

fizzBuzz :: Int -> String
fizzBuzz n | n `mod` 15 == 0  = "FizzBuzz"
       | n `mod` 3  == 0  = "Fizz"
       | n `mod` 5  == 0  = "Buzz"
       | otherwise        = show n

-- Sieve of Eratosthenes

sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]

primeNumbers :: [Integer] 
primeNumbers  = sieve [2..]
radius :: Double -> Double -> Double
radius x y = sqrt((x*x)+(y*y))

radius' :: (Double,Double) -> Double
radius' p = 
	let (x,y) = p 
	in sqrt((x*x)+(y*y))

sumEvens :: Integer -> Integer
sumEvens x = 
	if x `mod` 2 == 1 
	then sumEvens (x - 1)
	else if x < 2
	then 0
	else x + sumEvens (x - 2)

sumEvens' :: Integer -> Integer
sumEvens' x =
	if x < 2
	then 0
	else sum [0, 2..x]

collatz :: Integer -> Integer
collatz n =
	if n == 0 || n == 1
	then 1
	else if n > 1 && n `mod` 2 == 0
	then collatz (n `div` 2)
	else collatz ((3*n) + 1)

collatzCheck :: [Integer]
collatzCheck = map collatz [1, 2..100]

multiplesOfFive :: [Integer]
multiplesOfFive = map (*5) [1..20]

init' :: [Integer] -> [Integer]
init' [x] = []
init' (x:xs) = x: init' xs 

findEmpty:: [String] -> Bool
findEmpty [] = False
findEmpty (x:xs) = 
	if null x
	then True
	else findEmpty xs

getLengths :: [String] -> [Int]
getLengths x = map length x


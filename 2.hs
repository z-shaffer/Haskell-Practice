minList :: [Integer] -> Integer
minList [] = 0
minList [x] = x
minList (x:xs) = min x (minList xs)

addAbs :: [Integer] -> Integer
addAbs [] = 0
addAbs [x] = abs x
addAbs (x:xs) = abs x + addAbs xs

existsOdd :: [Integer] -> Bool
existsOdd [] = False
existsOdd (x:xs) = if odd x then True else existsOdd xs

findOdd :: [Integer] -> Maybe Integer
findOdd [] = Nothing
findOdd (x:xs) = 
	if odd x 
		then Just x 
	else findOdd xs

removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty x = filter (not . null) x

subtractEach :: [(Integer,Integer)] -> [Integer]
subtractEach [] = []
subtractEach ((a,b):xs) = a - b : subtractEach xs

makeGreeting :: Maybe String -> String
makeGreeting (Nothing) = "Hello!"	
makeGreeting (Just x) = "Hello, " ++ x ++ "!"

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x: catMaybes xs

classify :: [Either a b] -> ([a],[b])
classify [] = ([],[])
classify (Left x : list) = let (p1,p2) = classify list in (x:p1, p2)
classify (Right y : list) = let (p1,p2) = classify list in (p1, y:p2)

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = if x == y then isPrefix xs ys else False


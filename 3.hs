bubble :: Ord a => [a] -> [a]
bubble (x:y:xs) = if x > y then y: bubble (x:xs) else x:bubble (y:xs)
bubble xs = xs

bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort x = if x == bubble x then x else do bubbleSort(bubble x)

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = if x == y then isPrefix xs ys else False

findString :: String -> String -> Bool
findString xs [] = False
findString [] [] = True
findString xs (y:ys) = if isPrefix xs (y:ys) then True else isPrefix xs ys

genPrefix :: String -> [String]
genPrefix (z:zs) = map reverse (genPrefixHelper [z] zs) where 
	genPrefixHelper p [] = [p]
	genPrefixHelper p (x:xs) = p : genPrefixHelper (x:p) xs

genSubstrings :: String -> [String]
genSubstrings [] = [] ++ [""]
genSubstrings (x:xs) = genPrefix (x:xs) ++ genSubstrings xs

replacePrefix :: (String,String) -> String -> String
replacePrefix (x, y) z = if not (isPrefix x z) then z else y ++ drop (length x) z

replaceString :: (String,String) -> String -> String
replaceString (x,y) [] = []
replaceString (x,y) (z:zs) | isPrefix x (z:zs) = replacePrefix (x,y) (z:zs)
			   | otherwise = z : replaceString (x,y) zs

lookUp :: Char -> [(Char,Char)] -> Char
lookUp x [] = x
lookUp z ((x,y):xs) = if z == x then y else lookUp z xs

encode :: [(Char,Char)] -> String -> String
encode _ [] = []
encode [] x = x
encode ((x,y):xs) (z:zs) = lookUp z ((x,y):xs) : encode ((x,y):xs) zs

makeTable :: String -> String -> [(Char,Char)]
makeTable [] y = []
makeTable x [] = []
makeTable (x:[]) (y:[]) = [(x, y)]
makeTable (x:xs) (y:ys) = (x, y) : makeTable xs ys


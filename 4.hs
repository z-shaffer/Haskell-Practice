--Had issues with the tester and then had to resubmit again for a typo in the name of the function after trying to fix the tester issues. 
--Talked with Dr. Polonsky and he said it's okay and I shouldn't get a late penalty for it

mapPair :: (a -> b -> c) -> [(a,b)] -> [c]
mapPair f x = map (\r -> f (fst(r)) (snd(r))) x

mapPair' :: (a -> b -> c) -> [(b,a)] -> [c]
mapPair' f x = map (\r -> f (snd(r)) (fst(r))) x

diff :: [Integer] -> [Integer] -> [Integer]
diff x y = zipWith (-) x y

splice :: [String] -> [String] -> [String]
splice x y = zipWith (\r t -> r ++ t ++ r) x y

sqLens :: [String] -> [Integer]
sqLens x = map (\f -> (fromIntegral (length f)) ^2) x

bang :: [String] -> [String]
bang x = map (++ "!") x 

digitsOnly :: [Integer] -> [Integer]
digitsOnly x = filter (\y -> y >= 0 && y <= 9) x

removeXs :: [String] -> [String]
removeXs x = filter (\x -> null x || head x /= 'X') x

findNum :: Integer -> [Integer] -> Bool
findNum _ [] = False
findNum x (y:ys) = if x == y then True else findNum x ys

findNum' :: Integer -> [Integer] -> Bool
findNum' x ys = foldl (\acc y -> if x == y then True else acc) False ys

exists :: (a -> Bool) -> [a] -> Bool
exists f [] = False
exists f (x:xs) = if f x then True else exists f xs

exists' :: (a -> Bool) -> [a] -> Bool
exists' f xs = foldl (\acc x -> if f x then True else acc) False xs

noDups :: Eq a => [a] -> [a]
--noDups [] = []
--noDups (x:xs) = if x `elem` xs then noDups( x `filter` xs) else noDups xs
noDups _ = error "Did not finish"

noDups' :: Eq a => [a] -> [a]
--noDups' xs = foldl (\acc x -> if x `elem` acc then acc else acc ++ x) [] xs 
noDups' _ = error "Did not finish"

countOverflow :: Integer -> [String] -> Integer
countOverflow x [] = 0
countOverflow x (y:ys) = if (length y) > (fromIntegral x) then 1 + countOverflow x ys else countOverflow x ys

countOverflow' :: Integer -> [String] -> Integer
countOverflow' x ys = foldl (\acc y -> if fromIntegral (length y) > x then 1 + acc else acc) 0 ys 

concatList :: [[a]] -> [a]
concatList [] = []
concatList (x:y) = x ++ concatList y

concatList' :: [[a]] -> [a]
concatList' xs = foldl (\acc x -> acc ++ x) [] xs

bindList :: (a -> [b]) -> [a] -> [b]
bindList f [] = []
bindList f (x:xs) = f x ++ bindList f xs

bindList' :: (a -> [b]) -> [a] -> [b]
bindList' f xs = foldl (\acc x -> acc ++ (f x)) [] xs

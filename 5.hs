data LTree a = LLeaf a | LNode a (LTree a) (LTree a) 
   deriving Show

getLeaves :: LTree a -> [a]
getLeaves (LLeaf x) = [x]
getLeaves (LNode _ t1 t2) = getLeaves t1 ++ getLeaves t2

countNodes :: LTree a -> Integer
countNodes (LLeaf _) = 0
countNodes (LNode _ t1 t2) = 1 + countNodes t1 + countNodes t2

sumTree :: LTree Integer -> Integer
sumTree (LLeaf x) = x
sumTree (LNode root t1 t2) = root + sumTree t1 + sumTree t2

occursInLeaves :: (a -> Bool) -> LTree a -> Bool
occursInLeaves f (LLeaf x) = if f x then True else False
occursInLeaves f (LNode root t1 t2) = occursInLeaves f t1 || occursInLeaves f t2

checkNoCover :: (Eq a) => a -> LTree a -> Bool
checkNoCover x (LLeaf y) = x == y
checkNoCover x (LNode root t1 t2) = root /= x && (checkNoCover x t1 || checkNoCover x t2)

foldTree :: (a -> b -> b -> b) -> (a -> b) -> LTree a -> b
foldTree comb base (LLeaf x) = base x
foldTree comb base (LNode y t1 t2) = comb y (foldTree comb base t1)
					    (foldTree comb base t2)

getLeaves' :: LTree a -> [a]
getLeaves' = foldTree (\x y1 y2 -> y1 ++ y2) (:[])

countNodes' :: LTree a -> Integer
countNodes' = foldTree (\x y1 y2 -> 1 + y1 + y2) (const 0)

sumTree' :: LTree Integer -> Integer
sumTree' = foldTree (\x y1 y2 -> x + y1 + y2) (\d -> d)

occursInLeaves' :: (a -> Bool) -> LTree a -> Bool
occursInLeaves' = foldTree (\x y1 y2 -> y1 || y2) 

checkNoCover' :: (Eq a) => a -> LTree a -> Bool
checkNoCover' n = foldTree (\x y1 y2 -> x /= n && (y1 || y2)) (n==)

tests =
  [ ((getLeaves (LLeaf 1)) == [1])
  , ((getLeaves (LNode 2 (LLeaf 1) (LLeaf 3))) == [1,3])
  , ((getLeaves (LNode 4 (LNode 2 (LLeaf 1) (LLeaf 3)) (LLeaf 5))) == [1,3,5])
  , ((getLeaves (LNode "node 1" (LLeaf "leaf 1") (LNode "node 2" (LLeaf "leaf 2") (LLeaf "leaf 3")))) == ["leaf 1","leaf 2","leaf 3"])
  , ((getLeaves (LNode 'a' (LNode 'b' (LNode 'c' (LLeaf 'd') (LLeaf 'e')) (LLeaf 'f')) (LNode 'g' (LLeaf 'h') (LLeaf 'i')))) == "defhi")
  , ((countNodes (LLeaf 1)) == 0)
  , ((countNodes (LNode 2 (LLeaf 1) (LLeaf 3))) == 1)
  , ((countNodes (LNode 4 (LNode 2 (LLeaf 1) (LLeaf 3)) (LLeaf 5))) == 2)
  , ((countNodes (LNode "node 1" (LLeaf "leaf 1") (LNode "node 2" (LLeaf "leaf 2") (LLeaf "leaf 3")))) == 2)
  , ((countNodes (LNode 'a' (LNode 'b' (LNode 'c' (LLeaf 'd') (LLeaf 'e')) (LLeaf 'f')) (LNode 'g' (LLeaf 'h') (LLeaf 'i')))) == 4)
  , ((sumTree (LLeaf 1)) == 1)
  , ((sumTree (LNode 1 (LLeaf 2) (LLeaf 3))) == 6)
  , ((sumTree (LNode 1 (LLeaf 2) (LNode 3 (LLeaf 4) (LNode 5 (LLeaf 6) (LLeaf 7))))) == 28)
  , ((sumTree (LNode 1 (LNode 2 (LNode 3 (LLeaf 4) (LLeaf 5)) (LLeaf 6)) (LNode 7 (LLeaf 8) (LLeaf 9)))) == 45)
  , ((sumTree (LNode 10 (LNode 15 (LLeaf 20) (LNode 25 (LLeaf 5) (LLeaf 30))) (LNode 35 (LLeaf 40) (LLeaf 45)))) == 225)
  , ((occursInLeaves even (LLeaf 8)) == True)
  , ((occursInLeaves even (LLeaf 9)) == False)
  , ((occursInLeaves (== "hello") (LNode "hello" (LLeaf "again") (LLeaf "world"))) == False)
  , ((occursInLeaves (== "hello") (LNode "world" (LLeaf "again") (LLeaf "hello"))) == True)
  , ((occursInLeaves (\s -> elem (head s) "aeiou") (LNode "foo" (LLeaf "bar") (LNode "apple" (LLeaf "baz") (LLeaf "zap")))) == False)
  , ((occursInLeaves (\s -> elem (head s) "aeiou") (LNode "foo" (LLeaf "bar") (LNode "apple" (LLeaf "baz") (LLeaf "ink")))) == True)
  , ((checkNoCover 0 (LLeaf 0)) == True)
  , ((checkNoCover 0 (LNode 0 (LLeaf 0) (LLeaf 1))) == False)
  , ((checkNoCover 0 (LNode 1 (LLeaf 0) (LNode 0 (LLeaf 0) (LLeaf 0)))) == True)
  , ((checkNoCover 'a' (LNode 'a' (LLeaf 'b') (LNode 'c' (LLeaf 'd') (LLeaf 'e')))) == False)
  , ((checkNoCover 'a' (LNode 'z' (LNode 'y' (LLeaf 'x') (LLeaf 'a')) (LNode 'b' (LLeaf 'c') (LLeaf 'a')))) == True)
  , ((getLeaves' (LLeaf 1)) == [1])
  , ((getLeaves' (LNode 2 (LLeaf 1) (LLeaf 3))) == [1,3])
  , ((getLeaves' (LNode 4 (LNode 2 (LLeaf 1) (LLeaf 3)) (LLeaf 5))) == [1,3,5])
  , ((getLeaves' (LNode "node 1" (LLeaf "leaf 1") (LNode "node 2" (LLeaf "leaf 2") (LLeaf "leaf 3")))) == ["leaf 1","leaf 2","leaf 3"])
  , ((getLeaves' (LNode 'a' (LNode 'b' (LNode 'c' (LLeaf 'd') (LLeaf 'e')) (LLeaf 'f')) (LNode 'g' (LLeaf 'h') (LLeaf 'i')))) == "defhi")
  , ((countNodes' (LLeaf 1)) == 0)
  , ((countNodes' (LNode 2 (LLeaf 1) (LLeaf 3))) == 1)
  , ((countNodes' (LNode 4 (LNode 2 (LLeaf 1) (LLeaf 3)) (LLeaf 5))) == 2)
  , ((countNodes' (LNode "node 1" (LLeaf "leaf 1") (LNode "node 2" (LLeaf "leaf 2") (LLeaf "leaf 3")))) == 2)
  , ((countNodes' (LNode 'a' (LNode 'b' (LNode 'c' (LLeaf 'd') (LLeaf 'e')) (LLeaf 'f')) (LNode 'g' (LLeaf 'h') (LLeaf 'i')))) == 4)
  , ((sumTree' (LLeaf 1)) == 1)
  , ((sumTree' (LNode 1 (LLeaf 2) (LLeaf 3))) == 6)
  , ((sumTree' (LNode 1 (LLeaf 2) (LNode 3 (LLeaf 4) (LNode 5 (LLeaf 6) (LLeaf 7))))) == 28)
  , ((sumTree' (LNode 1 (LNode 2 (LNode 3 (LLeaf 4) (LLeaf 5)) (LLeaf 6)) (LNode 7 (LLeaf 8) (LLeaf 9)))) == 45)
  , ((sumTree' (LNode 10 (LNode 15 (LLeaf 20) (LNode 25 (LLeaf 5) (LLeaf 30))) (LNode 35 (LLeaf 40) (LLeaf 45)))) == 225)
  , ((occursInLeaves' even (LLeaf 8)) == True)
  , ((occursInLeaves' even (LLeaf 9)) == False)
  , ((occursInLeaves' (== "hello") (LNode "hello" (LLeaf "again") (LLeaf "world"))) == False)
  , ((occursInLeaves' (== "hello") (LNode "world" (LLeaf "again") (LLeaf "hello"))) == True)
  , ((occursInLeaves' (\s -> elem (head s) "aeiou") (LNode "foo" (LLeaf "bar") (LNode "apple" (LLeaf "baz") (LLeaf "zap")))) == False)
  , ((occursInLeaves' (\s -> elem (head s) "aeiou") (LNode "foo" (LLeaf "bar") (LNode "apple" (LLeaf "baz") (LLeaf "ink")))) == True)
  , ((checkNoCover' 0 (LLeaf 0)) == True)
  , ((checkNoCover' 0 (LNode 0 (LLeaf 0) (LLeaf 1))) == False)
  , ((checkNoCover' 0 (LNode 1 (LLeaf 0) (LNode 0 (LLeaf 0) (LLeaf 0)))) == True)
  , ((checkNoCover' 'a' (LNode 'a' (LLeaf 'b') (LNode 'c' (LLeaf 'd') (LLeaf 'e')))) == False)
  , ((checkNoCover' 'a' (LNode 'z' (LNode 'y' (LLeaf 'x') (LLeaf 'a')) (LNode 'b' (LLeaf 'c') (LLeaf 'a')))) == True)
  ]

main = putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
getErrors = map fst . filter (not . snd) . zip [1..] $ tests

type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
  deriving Show

mapAppend :: (a -> [b]) -> [a] -> [b]
mapAppend f [] = []
mapAppend f (x:xs) = f x ++ mapAppend f xs

addLetter :: Char -> [String] -> [String]
addLetter x [] = []
addLetter x y = map([x]++) y

addLetters :: [Char] -> [String] -> [String]
addLetters [] y = []
addLetters (x:xs) y = addLetter x y ++ addLetters xs y

makeWords :: [Char] -> Integer -> [String]
makeWords _ _ = [[]]

fv :: Prop -> [Vars]
fv (Var x) = [x]
fv (And x y) = (fv x) ++ (fv y)
fv (Or x y) = (fv x) ++ (fv y)
fv (Const x) = []
fv (Not x) = fv x

lookUp :: Vars -> [(Vars,Bool)] -> Bool
lookUp x ((y,z):zs) = if x == y then z else lookUp x zs

eval :: [(Vars,Bool)] -> Prop -> Bool
eval x (Or (Var y) (Var z)) = lookUp y x || lookUp z x
eval x (Or (Not(Var y)) (Var z)) = not (lookUp y x) || lookUp z x
eval x (Or (Var y) (Not(Var z))) = lookUp y x || not (lookUp z x)
eval x (Or (Not(Var y)) (Not(Var z))) = not (lookUp y x) || not (lookUp z x)
eval x (And (Var y) (Var z)) = lookUp y x && lookUp z x
eval x (And (Not(Var y)) (Var z)) = not (lookUp y x) && lookUp z x
eval x (And (Var y) (Not(Var z))) = lookUp y x && not (lookUp z x)
eval x (And (Not(Var y)) (Not(Var z))) = not (lookUp y x) && not (lookUp z x)

evalAll :: Prop -> [[(Vars,Bool)]] -> Bool
evalAll x [] = False
evalAll x (y:ys) = if eval y x then True else evalAll x ys

genEnvs :: [Vars] -> [[(Vars, Bool)]]
genEnvs x = sequence (map (\z->[(z,True), (z,False)]) x)

sat :: Prop -> Bool
sat (And (Var x) (Var y)) = True
sat (And (Not(Var x)) (Not(Var y))) = True
sat (Or (Var x) _) = True
sat (Or _ (Var y)) = True
sat _ = False


tests =
  [ ((mapAppend show [1,23,456]) == "123456")
  , ((mapAppend (\c -> c : " ") "hello") == "h e l l o ")
  , ((mapAppend (\s -> [take i s | i <- [1..3]]) ["hello","there","world"]) == ["h","he","hel","t","th","the","w","wo","wor"])
  , ((mapAppend (\s -> [elem c "aeiou" | c <- s]) ["hello","there","world"]) == [False,True,False,False,True,False,False,True,False,True,False,True,False,False,False])
  , ((mapAppend (\s -> [(c,i) | (c, i) <- zip s [1..]]) ["hello","again","world"]) == [('h',1),('e',2),('l',3),('l',4),('o',5),('a',1),('g',2),('a',3),('i',4),('n',5),('w',1),('o',2),('r',3),('l',4),('d',5)])
  , ((addLetter 'x' ["hey","again"]) == ["xhey","xagain"])
  , ((addLetter 'h' ["it","ill"]) == ["hit","hill"])
  , ((addLetter 't' ["ower","all","ale"]) == ["tower","tall","tale"])
  , ((addLetter 'b' ["ear","ent","old"]) == ["bear","bent","bold"])
  , ((addLetter 'l' ["air","earn","aw"]) == ["lair","learn","law"])
  , ((addLetters "abc" ["hey","again"]) == ["ahey","aagain","bhey","bagain","chey","cagain"])
  , ((addLetters "xyz" [""]) == ["x","y","z"])
  , ((addLetters "abc" ["1","2","3"]) == ["a1","a2","a3","b1","b2","b3","c1","c2","c3"])
  , ((addLetters "rt" ["est","on","ower"]) == ["rest","ron","rower","test","ton","tower"])
  , ((addLetters "bp" ["art","ear","ow","un"]) == ["bart","bear","bow","bun","part","pear","pow","pun"])
  , ((makeWords "abc" 2) == ["aa","ab","ac","ba","bb","bc","ca","cb","cc"])
  , ((makeWords "ab" 4) == ["aaaa","aaab","aaba","aabb","abaa","abab","abba","abbb","baaa","baab","baba","babb","bbaa","bbab","bbba","bbbb"])
  , ((makeWords "xyz" 3) == ["xxx","xxy","xxz","xyx","xyy","xyz","xzx","xzy","xzz","yxx","yxy","yxz","yyx","yyy","yyz","yzx","yzy","yzz","zxx","zxy","zxz","zyx","zyy","zyz","zzx","zzy","zzz"])
  , ((makeWords "z" 0) == [""])
  , ((makeWords "char" 1) == ["c","h","a","r"])
  , ((fv (Var "X" `And` Var "Y")) == ["X","Y"])
  , ((fv (Var "X" `And` Const True `Or` Var "Y")) == ["X","Y"])
  , ((fv ((Var "X" `And` Var "Y") `And` (Var "Z"))) == ["X","Y","Z"])
  , ((fv ((Not (Var "X") `Or` Not (Var "Z")) `And` (Not (Var "Y") `Or` Var "Z"))) == ["X","Z","Y"])
  , ((fv (Not (Var "X") `Or` (Var "X") `And` Var "Y")) == ["X","Y"])
  , ((lookUp "X" [("X",True),("Y",False)]) == True)
  , ((lookUp "X" [("W",False),("X",True),("Y",False)]) == True)
  , ((lookUp "Y" [("W",False),("X",True),("Y",False)]) == False)
  , ((lookUp "Z" [("W",False),("X",True),("Y",False),("Z",False)]) == False)
  , ((lookUp "Z" [("W",False),("X",True),("Y",False),("Z",True)]) == True)
  , ((eval [("X",False),("Y",True) ] (Var "X" `And` Var "Y")) == False)
  , ((eval [("X",False),("Y",True) ] (Var "X" `Or` Var "Y")) == True)
  , ((eval [("X",False),("Y",True) ] (Not (Var "X") `Or` (Var "Y"))) == True)
  , ((eval [("X",True) ,("Y",False)] (Not (Var "X") `Or` (Var "Y"))) == False)
  , ((eval [("X",True) ,("Y",True) ] (Not (Var "X") `Or` Not (Var "Y"))) == False)
  , ((evalAll (Var "X" `And` Var "Y") [[("X",False),("Y",True)]]) == False)
  , ((evalAll (Var "X" `And` Var "Y") [[("X",False),("Y",True)],[("X",True),("Y",True)]]) == True)
  , ((evalAll (Var "X" `Or` Var "Y") [[("X",False),("Y",True)]]) == True)
  , ((evalAll (Var "X" `Or` Var "Y") []) == False)
  , ((evalAll (Not (Var "X") `Or` Not (Var "Y")) [[("X",False),("Y",True)],[("X",True),("Y",True)]]) == True)
  , ((evalAll (Not (Var "X") `Or` Not (Var "Y")) [[("X",True) ,("Y",True)]]) == False)
  , ((genEnvs ["X"]) == [[("X",True)],[("X",False)]])
  , ((genEnvs ["X","Y"]) == [[("X",True),("Y",True)],[("X",True),("Y",False)],[("X",False),("Y",True)],[("X",False),("Y",False)]])
  , ((genEnvs []) == [[]])
  , ((genEnvs ["X","Y","Z"]) == [[("X",True),("Y",True),("Z",True)],[("X",True),("Y",True),("Z",False)],[("X",True),("Y",False),("Z",True)],[("X",True),("Y",False),("Z",False)],[("X",False),("Y",True),("Z",True)],[("X",False),("Y",True),("Z",False)],[("X",False),("Y",False),("Z",True)],[("X",False),("Y",False),("Z",False)]])
  , ((genEnvs ["X","Y","Z","W"]) == [[("X",True),("Y",True),("Z",True),("W",True)],[("X",True),("Y",True),("Z",True),("W",False)],[("X",True),("Y",True),("Z",False),("W",True)],[("X",True),("Y",True),("Z",False),("W",False)],[("X",True),("Y",False),("Z",True),("W",True)],[("X",True),("Y",False),("Z",True),("W",False)],[("X",True),("Y",False),("Z",False),("W",True)],[("X",True),("Y",False),("Z",False),("W",False)],[("X",False),("Y",True),("Z",True),("W",True)],[("X",False),("Y",True),("Z",True),("W",False)],[("X",False),("Y",True),("Z",False),("W",True)],[("X",False),("Y",True),("Z",False),("W",False)],[("X",False),("Y",False),("Z",True),("W",True)],[("X",False),("Y",False),("Z",True),("W",False)],[("X",False),("Y",False),("Z",False),("W",True)],[("X",False),("Y",False),("Z",False),("W",False)]])
  , ((sat (Var "X" `And` Var "Y")) == True)
  , ((sat (Var "X" `And` (Not $ Var "X"))) == False)
  , ((sat (Var "X" `Or` (Var "Y" `And` Not (Var "X")))) == True)
  , ((sat (Var "Y" `And` (Not (Var "Y")) `Or` Var "X")) == True)
  , ((sat ((Not $ Var "X") `And` (Not $ Var "Y") `And` (Not $ Var "Z"))) == True)
  ]

main = putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
getErrors = map fst . filter (not . snd) . zip [1..] $ tests

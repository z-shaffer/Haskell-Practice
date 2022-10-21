-- 1) 
-- List:
-- Null
-- foldl
-- foldr
-- ++
-- insert
--
-- Char:
-- isUpper
-- isLower
-- isDigit
-- isAlpha
-- isNumber
--
-- 2)

import Data.Char;
import Data.List;

type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
	| Imp Prop Prop | Iff Prop Prop | Xor Prop Prop
	deriving (Show,Eq)

prop1 = Var "X" `And` Var "Y" -- X /\ Y
prop2 = Var "X" `Imp` Var "Y" -- X -> Y
prop3 = Not (Var "X") `Or` (Var "Y") -- !X \/ Y
prop4 = Not (Var "X") `Iff` Not (Var "Y") -- !X <-> !Y


type Env = [(Vars,Bool)]

lookUp :: Vars -> [(Vars,Bool)] -> Bool
lookUp x [] = error ("Cannot find variable " ++ x ++ " in the environment.")
lookUp x ((key,val):r) = if key == x then val else lookUp x r

eval :: Env -> Prop -> Bool
eval e (Var x) = case (lookup x e) of
    Nothing -> error $ "No value for variable " ++ x
    Just v  -> v 
eval e (Const b) = b
eval e (And f1 f2) = eval e f1 && eval e f2
eval e (Or f1 f2) = eval e f1 || eval e f2
eval e (Iff f1 f2) = eval e f1 == eval e f2
eval e (Imp f1 f2) = not (eval e f1) || (eval e f1 && eval e f2)
eval e (Xor f1 f2) = if eval e f1 /= eval e f2 then True else False
eval e (Not f) = not (eval e f)

-- binary operators
data BOps = AndOp | OrOp | ImpOp | IffOp | XorOp
 deriving (Show,Eq)

-- the type of tokens
data Token = VSym Vars | CSym Bool | BOp BOps | NotOp | LPar | RPar
	| PB Prop -- a token to store parsed boolean expressions
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer "" = []
lexer ('t':'t':s) = CSym True : lexer s
lexer ('f':'f':s) = CSym False : lexer s
lexer ('(':s) = LPar : lexer s
lexer (')':s) = RPar : lexer s
lexer ('/':'\\':s) = BOp AndOp : lexer s
lexer ('\\':'/':s) = BOp OrOp : lexer s
lexer ('!':s) = NotOp : lexer s
lexer ('-':'>':s) = BOp ImpOp : lexer s
lexer ('<':'-':'>':s) = BOp IffOp : lexer s
lexer ('<':'+':'>':s) = BOp XorOp : lexer s
lexer (c:s) | isUpper c = let (d,e) = span isAlphaNum s in VSym (c : d) : lexer e
lexer (c:s) | isSpace c = lexer s 
lexer s = error ("Unrecognized token: " ++ s)

parseProp :: [Token] -> Prop
parseProp t = sr [] t

sr :: [Token] -> [Token] -> Prop
sr [PB e] [] = e
sr (VSym v : s) q = sr(PB(Var v) : s) q -- R1
sr (CSym n : s) q = sr (PB (Const n)   : s) q -- R2
sr (PB e1 : NotOp : s) q = sr (PB (Not e1) : s) q -- R8
sr (PB e2 : BOp AndOp : PB e1 : s) q = sr (PB (And e1 e2) : s) q -- R3
sr (PB e2 : BOp OrOp : PB e1 : s) q = sr (PB (Or e1 e2) : s) q -- R4
sr (PB e2 : BOp XorOp : PB e1 : s) q = sr (PB (Xor e1 e2) : s) q -- R5
sr (PB e2 : BOp ImpOp : PB e1 : s) q = sr (PB (Imp e1 e2) : s) q -- R6
sr (PB e2 : BOp IffOp : PB e1 : s) q = sr (PB (Iff e1 e2) : s) q -- R7
sr (RPar : PB e : LPar : s) q = sr (PB e : s) q
sr s (q:qs) = sr (q:s) qs -- shift stage
sr s [] = error ("Parse error:" ++ show s)

genEnvs :: [Vars] -> [[(Vars,Bool)]]
genEnvs = foldr (\x y -> map ((x,True):) y ++ map ((x,False):) y) [[]]

fv :: Prop -> [Vars]
fv (Var x) = [x]
fv (And f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Or  f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Not f)     = fv f
fv (Imp f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Iff f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Xor f1 f2) = removeDups (fv f1 ++ fv f2)
fv _ = []

removeDups :: (Eq a) => [a] -> [a]
removeDups = foldr (\x -> (x :) . filter (/= x)) []

findSat :: Prop -> Maybe Env 
findSat x = find (\y -> eval y x) (genEnvs (fv x))

solve :: String -> String
solve x = case (findSat(parseProp(lexer x))) of 
	(Just e) -> "Satisfiable."
	(Nothing) -> "No solution."

tests =
  [ ((eval [("E",True)] (And (And (Const True) (Imp (Var "E") (Var "E"))) (Const False))) == False)
  , ((eval [("E",True),("A",False)] (Not (Xor (Imp (Var "E") (Var "A")) (Var "A")))) == True)
  , ((eval [] (Const True)) == True)
  , ((eval [("P",False),("T",False)] (Xor (Iff (And (Var "P") (Var "P")) (Xor (Var "T") (Var "P"))) (Const False))) == True)
  , ((eval [("P",True),("T",True)] (Imp (And (Var "T") (Imp (Var "T") (Var "P"))) (Const True))) == True)
  , ((eval [("D",False)] (Not (Const False))) == True)
  , ((eval [("P",False),("T",True)] (Var "P")) == False)
  , ((eval [("A",True)] (Var "A")) == True)
  , ((eval [("X",True),("Y",False),("O",False),("E",False),("A",True),("D",False)] (And (Or (Or (Var "A") (Const False)) (Xor (Const False) (Const False))) (Var "X"))) == True)
  , ((eval [] (Const False)) == False)
  , ((lexer "((tt) /\\ ((Z) -> (D))) /\\ (ff)") == [LPar,LPar,CSym True,RPar,BOp AndOp,LPar,LPar,VSym "Z",RPar,BOp ImpOp,LPar,VSym "D",RPar,RPar,RPar,BOp AndOp,LPar,CSym False,RPar])
  , ((lexer "!(((B) -> (H)) <+> (Z))") == [NotOp,LPar,LPar,LPar,VSym "B",RPar,BOp ImpOp,LPar,VSym "H",RPar,RPar,BOp XorOp,LPar,VSym "Z",RPar,RPar])
  , ((lexer "tt") == [CSym True])
  , ((lexer "(((C) /\\ (V)) <-> ((R) <+> (X))) <+> (ff)") == [LPar,LPar,LPar,VSym "C",RPar,BOp AndOp,LPar,VSym "V",RPar,RPar,BOp IffOp,LPar,LPar,VSym "R",RPar,BOp XorOp,LPar,VSym "X",RPar,RPar,RPar,BOp XorOp,LPar,CSym False,RPar])
  , ((lexer "((O) /\\ ((D) -> (Q))) -> (tt)") == [LPar,LPar,VSym "O",RPar,BOp AndOp,LPar,LPar,VSym "D",RPar,BOp ImpOp,LPar,VSym "Q",RPar,RPar,RPar,BOp ImpOp,LPar,CSym True,RPar])
  , ((lexer "!(ff)") == [NotOp,LPar,CSym False,RPar])
  , ((lexer "J") == [VSym "J"])
  , ((lexer "Z") == [VSym "Z"])
  , ((lexer "(((G) \\/ (ff)) \\/ ((ff) <+> (ff))) /\\ (P)") == [LPar,LPar,LPar,VSym "G",RPar,BOp OrOp,LPar,CSym False,RPar,RPar,BOp OrOp,LPar,LPar,CSym False,RPar,BOp XorOp,LPar,CSym False,RPar,RPar,RPar,BOp AndOp,LPar,VSym "P",RPar])
  , ((lexer "ff") == [CSym False])
  , ((parseProp [LPar,LPar,CSym True,RPar,BOp AndOp,LPar,LPar,VSym "Z",RPar,BOp ImpOp,LPar,VSym "D",RPar,RPar,RPar,BOp AndOp,LPar,CSym False,RPar]) == And (And (Const True) (Imp (Var "Z") (Var "D"))) (Const False))
  , ((parseProp [NotOp,LPar,LPar,LPar,VSym "B",RPar,BOp ImpOp,LPar,VSym "H",RPar,RPar,BOp XorOp,LPar,VSym "Z",RPar,RPar]) == Not (Xor (Imp (Var "B") (Var "H")) (Var "Z")))
  , ((parseProp [CSym True]) == Const True)
  , ((parseProp [LPar,LPar,LPar,VSym "C",RPar,BOp AndOp,LPar,VSym "V",RPar,RPar,BOp IffOp,LPar,LPar,VSym "R",RPar,BOp XorOp,LPar,VSym "X",RPar,RPar,RPar,BOp XorOp,LPar,CSym False,RPar]) == Xor (Iff (And (Var "C") (Var "V")) (Xor (Var "R") (Var "X"))) (Const False))
  , ((parseProp [LPar,LPar,VSym "O",RPar,BOp AndOp,LPar,LPar,VSym "D",RPar,BOp ImpOp,LPar,VSym "Q",RPar,RPar,RPar,BOp ImpOp,LPar,CSym True,RPar]) == Imp (And (Var "O") (Imp (Var "D") (Var "Q"))) (Const True))
  , ((parseProp [NotOp,LPar,CSym False,RPar]) == Not (Const False))
  , ((parseProp [VSym "J"]) == Var "J")
  , ((parseProp [VSym "Z"]) == Var "Z")
  , ((parseProp [LPar,LPar,LPar,VSym "G",RPar,BOp OrOp,LPar,CSym False,RPar,RPar,BOp OrOp,LPar,LPar,CSym False,RPar,BOp XorOp,LPar,CSym False,RPar,RPar,RPar,BOp AndOp,LPar,VSym "P",RPar]) == And (Or (Or (Var "G") (Const False)) (Xor (Const False) (Const False))) (Var "P"))
  , ((parseProp [CSym False]) == Const False)
  , ((findSat (And (And (Const True) (Imp (Var "Z") (Var "D"))) (Const False))) == Nothing)
  , ((findSat (Not (Xor (Imp (Var "B") (Var "H")) (Var "Z")))) == Just [("B",True),("H",True),("Z",True)])
  , ((findSat (Const True)) == Just [])
  , ((findSat (Xor (Iff (And (Var "C") (Var "V")) (Xor (Var "R") (Var "X"))) (Const False))) == Just [("C",True),("V",True),("R",True),("X",False)])
  , ((findSat (Imp (And (Var "O") (Imp (Var "D") (Var "Q"))) (Const True))) == Just [("O",True),("D",True),("Q",True)])
  , ((findSat (Not (Const False))) == Just [])
  , ((findSat (Var "J")) == Just [("J",True)])
  , ((findSat (Var "Z")) == Just [("Z",True)])
  , ((findSat (And (Or (Or (Var "G") (Const False)) (Xor (Const False) (Const False))) (Var "P"))) == Just [("G",True),("P",True)])
  , ((findSat (Const False)) == Nothing)
  , ((solve "((tt) /\\ ((Z) -> (D))) /\\ (ff)") == "No solution.")
  , ((solve "!(((B) -> (H)) <+> (Z))") == "Satisfiable.")
  , ((solve "tt") == "Satisfiable.")
  , ((solve "(((C) /\\ (V)) <-> ((R) <+> (X))) <+> (ff)") == "Satisfiable.")
  , ((solve "((O) /\\ ((D) -> (Q))) -> (tt)") == "Satisfiable.")
  , ((solve "!(ff)") == "Satisfiable.")
  , ((solve "J") == "Satisfiable.")
  , ((solve "Z") == "Satisfiable.")
  , ((solve "(((G) \\/ (ff)) \\/ ((ff) <+> (ff))) /\\ (P)") == "Satisfiable.")
  , ((solve "ff") == "No solution.")
  ]

main = putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)

getErrors = map fst . filter (not . snd) . zip [1..] $ tests

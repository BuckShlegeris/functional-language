{-# Language OverloadedStrings, TypeSynonymInstances, FlexibleInstances#-}

import Reduce
import DataStructures


--data Exp = Op Operator Exp Exp
--         | Const Obj
--         | Name Name
--         | Let Name Exp Exp
--         | Def Name [Name] Exp Exp
--         | Call Exp [Exp]
--         | Case Exp Name [Name] Exp Exp
--         | Undefined

--data Obj = Number Integer
--         | String String
--         | Function [Name] Exp
--         | Struct Name [Obj]

num = Const . Number
one = num 1
two = num 2
three = num 3

myIf cond exp1 exp2 = (Case cond "True" [] exp1 exp2)

compileModule :: [(Exp -> Exp)] -> Exp -> Exp
compileModule [] final = final
compileModule (x:xs) final = x (compileModule xs final)

factorialFunc = (Def "factorial" ["x"]
              (myIf (Op Equals "x" one)
                (one)
                (Op Multiply (Call ("factorial") [Op Minus ("x") (one)]) ("x"))))

factorial = compileModule [factorialFunc] (Call (Name "factorial") [num 6])

emptyList = Const $ Struct "EL" []

cons = Function ["x", "y"] (Const $ Struct "Cons" ["x", "y"])

myLength = Function ["x"] (Case "x" "Cons" ["_","xs"]
                  (Op Plus (Call "myLength" ["xs"]) one)
                  (num 0))

thing = (Let "myLength" (Const myLength) (
         Let "cons" (Const cons) (
         Call "myLength" [Call "cons" [one,emptyList]])))

assert s True = putStrLn s
assert s False = error $ "Failed assertion: "++s

main = do
  assert "Factorial works" ((show $ eval factorial) == "720")
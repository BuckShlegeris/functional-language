{-# Language OverloadedStrings, TypeSynonymInstances, FlexibleInstances#-}

module DataStructures where

import Data.String (IsString, fromString)
import Data.List (intersperse)


data Exp = Op Operator Exp Exp
         | Const Obj
         | Name Name
         | Let Name Exp Exp
         | Def Name [Name] Exp Exp
         | Call Exp [Exp]
         | Case Exp Name [Name] Exp Exp
         | Undefined
      deriving (Eq)

instance IsString Exp where
  fromString x = Name x

instance Show Exp where
    show exp = case exp of
      (Op op exp1 exp2) -> "(# # #)" %% [s exp1, s op, s exp2]
      (Const x) -> show x
      (Name n) -> n
      (Let name exp1 exp2) -> "(let # # #)" %% [name, s exp1, s exp2]
      (Def name args exp1 exp2) -> "(def #(#) # #)" %%
                            [name, concat $ intersperse ", " (map show args),
                               s exp1, s exp2]
      (Call func args) -> "#(#)" %% [s func, concat $ intersperse ", " (map show args)]
      (Case exp0 name args exp1 exp2) -> "(case # of (# #) # #)" %%
                            [s exp0, name, concat $ intersperse " " (map show args),
                               s exp1, s exp2]

data Obj = Number Integer
         | String String
         | Function [Name] Exp
         | Struct Name [Obj]
      deriving (Eq)

instance IsString Obj where
  fromString x = Struct x []

instance Show Obj
  where
    show obj = case obj of
      (Number n) -> show n
      (Function args body) -> "(lambda (#) #)" %%
                            [concat $ intersperse ", " (map show args),
                               s body]
      (Struct name objs)
          | length objs == 0 -> name
          | otherwise -> "(##)" %%
                            [name, concatMap (\x->' ':x) (map show objs)]
      (String x) -> "\"%\"" %% [x]

s :: Show a => a -> String
s = show


data Operator = Plus | Minus | Multiply | Mod | Equals | Concat
      deriving (Show, Eq)


(%%) :: String -> [String] -> String
template %% [] = template
('#':template) %% (x:xs) = x ++ (template %% xs)
(x:template) %% stuff = x : (template %% stuff)


type Name = String
type Context = [(Name, Obj)]

operate :: Operator -> Obj -> Obj -> Obj
operate op = case op of
    Plus -> wrapFunc (+)
    Minus -> wrapFunc (-)
    Multiply -> wrapFunc (*)
    Mod -> wrapFunc mod
    Equals -> (\x y -> if x==y then Struct "True" [] else Struct "False" [])
    Concat -> (\(String x) (String y) -> (String $ x ++ y))

wrapFunc :: (Integer -> Integer -> Integer) -> (Obj -> Obj -> Obj)
wrapFunc f = (\(Number x) (Number y) -> (Number $ f x y))


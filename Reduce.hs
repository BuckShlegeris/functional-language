{-# Language OverloadedStrings, TypeSynonymInstances, FlexibleInstances#-}

module Reduce where

import DataStructures
import Debug.Trace (traceShow, trace)

p x = traceShow x x


getFromContext :: Context -> Name -> Obj
getFromContext [] name = error ("Name error: "++name)
getFromContext ((name, obj):xs) thisName =
        if name == thisName then obj else getFromContext xs thisName

addToContext context name val = (name,val):context

evalWithContext :: Int -> Context -> Exp -> Obj
evalWithContext depth context exp = case exp of
      (Op op lhs rhs) -> operate op (evalWithContext (depth+1) context lhs)
                                    (evalWithContext (depth+1) context rhs)
      (Const x) -> x
      (Name name) -> getFromContext context name
      (Let name exp1 exp2) -> evalWithContext (depth+1)
                    (addToContext context name (evalWithContext (depth+1) context exp1))
                          exp2
      (Def func args exp exp2) -> evalWithContext (depth+1)
                    (addToContext context func (Function args exp))
                          exp2
      (Call func args) -> call depth context (evalWithContext (depth+1) context func) args
      (Case thing name argNames exp1 exp2) -> case (evalWithContext (depth+1) context thing) of
          (Struct name2 objs)
              | name == name2 -> callCase depth context argNames objs exp1
              | otherwise -> evalWithContext (depth+1) context exp2
          obj -> error $ "invalid subject of case statement: "++show obj
          where
            valOfThing = evalWithContext (depth+1) context thing
      (Undefined) -> undefined

render :: Int -> Context -> Exp -> String
-- render depth context exp = start ++ padding (80 - length start) ++show (map fst context)
--  where
--    start = padding depth ++ show exp
--    padding x = take x (repeat ' ')
render depth context exp = show exp


call :: Int -> Context -> Obj -> [Exp] -> Obj
call depth context (Function argNames body) args
            = evalWithContext (depth+1) (bindArgs context argNames args) body
      where
        bindArgs :: Context -> [Name] -> [Exp] -> Context
        bindArgs context [] [] = context
        bindArgs _ [] _ = error "too many arguments"
        bindArgs _ _ [] = error "too few arguments"
        bindArgs context (name:names) (arg:args)
              = (name, evalWithContext depth context arg):bindArgs context names args

callCase :: Int -> Context -> [Name] -> [Obj] -> Exp -> Obj
callCase depth context names objs exp = evalWithContext (depth+1) (bindArgs context names objs) exp
      where
        bindArgs :: Context -> [Name] -> [Obj] -> Context
        bindArgs context names objs = (zip names objs) ++ context

eval :: Exp -> Obj
eval = evalWithContext 0 []
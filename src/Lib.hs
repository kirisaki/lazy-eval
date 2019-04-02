module Lib
    ( someFunc
    ) where

import Control.Monad.Writer
import Control.Exception.Safe
import Data.Monoid

someFunc :: IO ()
someFunc = putStrLn "someFunc"



type Eval a = Writer (Sum Int) a

type Func = Exp -> Eval Exp
type Func2 = Exp -> Exp -> Eval Exp
type Func3 = Exp -> Exp -> Exp -> Eval Exp

data Exp
  = Bool Bool
  | Func Func
  | Func2 Func2
  | Func3 Func3
  | Eff
  | Apply Exp Exp
  | Nil

eval :: Func
eval Nil= pure Nil
eval v@(Bool _) = pure v
eval v@(Func _) = pure v
eval v@(Func2 _) = pure v 
eval v@(Func3 _) = pure v 
eval Eff = tell 1 >> pure Nil
eval (Apply (Func f) x) = f x
eval (Apply (Func2 f) x) = pure . Func $ f x
eval (Apply (Func3 f) x) = pure . Func2 $ f x
eval (Apply _ _) = undefined

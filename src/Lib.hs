module Lib
    ( someFunc
    ) where

import Control.Monad.Writer
import Control.Exception.Safe
import Data.Monoid
import GHC.IO.Unsafe

someFunc :: IO ()
someFunc = print $ runWriter (eval tree)



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
  | Val Exp

instance Show Exp where
  show (Bool v) = "(" ++ show v ++ ")"
  show (Func _) =  "(Func)"
  show (Func2 _) = "(Func2)"
  show (Func3 _) = "(Func3)"
  show Eff = "(Eff)"
  show (Apply x y) = "(Apply) " ++ show x ++ " " ++ show y
  show Nil = "(Nil)"
  show (Val _) = "(Val)"

eval :: Func
eval Nil= pure Nil
eval v@(Bool _) = unsafePerformIO $ putStrLn "> Bool" >> (pure $ pure v)
eval v@(Func _) = unsafePerformIO $ putStrLn "> Func" >> (pure $ pure v)
eval v@(Func2 _) = unsafePerformIO $ putStrLn "> Func2" >> (pure $ pure v)
eval v@(Func3 _) = unsafePerformIO $ putStrLn "> Func3" >> (pure $ pure v)
eval (Val exp) = unsafePerformIO $ putStrLn "> Val" >> (pure $ eval exp)
eval Eff = tell (Sum 1) >> (unsafePerformIO $ putStrLn "> Eff" >> (pure $ pure Nil))
eval (Apply (Func f) x) = unsafePerformIO $ putStrLn ("> Apply Func1 " ++ show x) >> (pure $ f x >>= eval)
eval (Apply (Func2 f) x) = unsafePerformIO $ putStrLn ("> Apply Func2 " ++ show x) >> (pure $ pure . Func $ f x)
eval (Apply (Func3 f) x) = unsafePerformIO $ putStrLn ("> Apply Func3 " ++ show x) >> (pure $ pure . Func2 $ f x)
eval (Apply exp@(Apply _ _) x) = do
  e <-   unsafePerformIO $ putStrLn ("> Apply Apply " ++ show exp ++ " " ++ show x) >> (pure $ eval exp)
  eval $ Apply e x
eval (Apply x y) = pure . unsafePerformIO $ putStrLn ("failed: " ++ show x ++ " " ++ show y) >> pure Nil

if_ :: Func3
if_ (Bool cond) x y = if cond then pure x else pure y

tree = Apply (Apply (Apply (Func3 if_) (Bool False)) (Val Eff)) Nil

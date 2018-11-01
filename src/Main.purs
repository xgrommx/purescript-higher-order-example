module Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Leibniz (type (~), coerceSymm)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Expr (Expr, add_, bool_, equal_, if_, mul_, not_, val_)
import ExprF (ExprF(..))
import Higher (HAlgebra, hcata)
import Partial.Unsafe (unsafePartial)

data Value a
  = VInt Int (a ~ Int)
  | VBool Boolean (a ~ Boolean)

instance showValue :: Show a => Show (Value a) where
  show = case _ of
    VInt a p -> "VInt " <> show (coerceSymm p a)
    VBool a p -> "VBool " <> show (coerceSymm p a)

evalAlgebra :: HAlgebra ExprF Identity
evalAlgebra = case _ of
  ValF x p -> Identity $ coerceSymm p x
  BoolF x p -> Identity $ coerceSymm p x
  MulF x y p -> Identity $ coerceSymm p $ unwrap x * unwrap y
  AddF x y p -> Identity $ coerceSymm p $ unwrap x + unwrap y
  EqualF x y p -> Identity $ coerceSymm p $ unwrap x == unwrap y
  NotF x p -> Identity $ coerceSymm p $ not $ unwrap x
  LessThanF x y p -> Identity $ coerceSymm p $ unwrap x < unwrap y
  IfF c x y p -> Identity $ if (unwrap c) then (unwrap x) else (unwrap y)

evalValueAlgebra :: Partial => HAlgebra ExprF Value
evalValueAlgebra = case _ of
  ValF x p -> VInt x p
  BoolF x p -> VBool x p
  MulF (VInt x _) (VInt y _) p -> VInt (x * y) p
  AddF (VInt x _) (VInt y _) p -> VInt (x + y) p
  EqualF (VInt x _) (VInt y _) p -> VBool (x == y) p
  NotF (VBool x _) p -> VBool (not x) p
  LessThanF (VInt x _) (VInt y _) p -> VBool (x < y) p
  IfF (VBool c _) x y p -> if c then x else y

value :: Expr Boolean
value = not_ (equal_ (mul_ (val_ 10) (val_ 1)) (add_ (val_ 0) (val_ 1)))

value2 :: Expr Int
value2 = if_ (bool_ false) (val_ 1) (add_ (val_ 42) (val_ 45))

main :: Effect Unit
main = do
  logShow value
  logShow $ hcata evalAlgebra value
  logShow $ hcata evalAlgebra value2
  logShow $ hcata (unsafePartial evalValueAlgebra) value
  logShow $ hcata (unsafePartial evalValueAlgebra) value2

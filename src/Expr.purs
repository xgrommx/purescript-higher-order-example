module Expr where

import Prelude

import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Leibniz (type (~))
import Data.Newtype (unwrap)
import ExprF (ExprF(..))
import Higher (class HCorecursive, class HRecursive, HAlgebra, hcata)
import Prettier.Printer (DOC, pretty, text, (<+>))

data Expr a
  = Add (Expr Int) (Expr Int) (a ~ Int)
  | Mul (Expr Int) (Expr Int) (a ~ Int)
  | Equal (Expr Int) (Expr Int) (a ~ Boolean)
  | Not (Expr Boolean) (a ~ Boolean)
  | Val Int (a ~ Int)
  | Bool Boolean (a ~ Boolean)
  | LessThan (Expr Int) (Expr Int) (a ~ Boolean)
  | If (Expr Boolean) (Expr a) (Expr a) (Either (a ~ Int) (a ~ Boolean))

not_ :: Expr Boolean -> Expr Boolean
not_ a = Not a identity

equal_ :: Expr Int -> Expr Int -> Expr Boolean
equal_ a b = Equal a b identity

mul_ :: Expr Int -> Expr Int -> Expr Int
mul_ a b = Mul a b identity

add_ :: Expr Int -> Expr Int -> Expr Int
add_ a b = Add a b identity

val_ :: Int -> Expr Int
val_ a = Val a identity

bool_ :: Boolean -> Expr Boolean
bool_ a = Bool a identity

lessThan_ :: Expr Int -> Expr Int -> Expr Boolean
lessThan_ a b = LessThan a b identity

if_ :: Expr Boolean -> Expr Int -> Expr Int -> Expr Int
if_ c a b = If c a b (Left identity)

instance hrecursiveExpr :: HRecursive Expr ExprF where
  hproject = case _ of
    Add x y p -> AddF x y p
    Mul x y p -> MulF x y p
    Equal x y p -> EqualF x y p
    Val x p -> ValF x p
    Bool x p -> BoolF x p
    Not x p -> NotF x p
    LessThan x y p -> LessThanF x y p
    If c x y p -> IfF c x y p

instance hcorecursiveExpr :: HCorecursive Expr ExprF where
  hembed = case _ of
    AddF x y p -> Add x y p
    MulF x y p -> Mul x y p
    EqualF x y p -> Equal x y p
    ValF x p -> Val x p
    BoolF x p -> Bool x p
    NotF x p -> Not x p
    LessThanF x y p -> LessThan x y p
    IfF c x y p -> If c x y p

instance showExpr :: Show a => Show (Expr a) where
  show = pretty 1 <<< unwrap <<< hcata docAlgebra 
    where
      docAlgebra :: HAlgebra ExprF (Const DOC)
      docAlgebra = case _ of
        ValF x _ -> Const <<< text $ show x
        BoolF x _ -> Const <<< text $ show x
        AddF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "+" <+> y
        MulF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "*" <+> y
        EqualF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "==" <+> y
        NotF (Const x) _ -> Const $ text "not" <+> x
        LessThanF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "<" <+> y
        IfF (Const c) (Const x) (Const y) _ -> Const $ text "if" <+> c <+> text "then" <+> x <+> text "else" <+> y
      parens :: DOC -> DOC
      parens p = text "(" <> p <> text ")"  
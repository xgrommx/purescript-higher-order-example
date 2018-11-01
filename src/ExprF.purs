module ExprF where

import Prelude

import Control.Apply (lift2, lift3)
import Data.Either (Either)
import Data.Leibniz (type (~))
import Higher (class HFoldable, class HFunctor, class HTraversable, hfoldlDefault, hfoldrDefault)

data ExprF h a
  = AddF (h Int) (h Int) (a ~ Int)
  | MulF (h Int) (h Int) (a ~ Int)
  | EqualF (h Int) (h Int) (a ~ Boolean)
  | NotF (h Boolean) (a ~ Boolean)
  | ValF Int (a ~ Int)
  | BoolF Boolean (a ~ Boolean)
  | LessThanF (h Int) (h Int) (a ~ Boolean)
  | IfF (h Boolean) (h a) (h a) (Either (a ~ Int) (a ~ Boolean))

instance hfunctorExpr :: HFunctor ExprF where
  hmap f = case _ of
    AddF x y p -> AddF (f x) (f y) p
    MulF x y p -> MulF (f x) (f y) p
    EqualF x y p -> EqualF (f x) (f y) p
    ValF x p -> ValF x p
    BoolF x p -> BoolF x p
    NotF x p -> NotF (f x) p
    LessThanF x y p -> LessThanF (f x) (f y) p
    IfF c x y p -> IfF (f c) (f x) (f y) p  

instance hfoldableExpr :: HFoldable ExprF where
  hfoldr f = hfoldrDefault f
  hfoldl f = hfoldlDefault f
  hfoldMap f = case _ of
    AddF x y p -> (f x) <> (f y)
    MulF x y p -> (f x) <> (f y)
    EqualF x y p -> (f x) <> (f y)
    ValF x p -> mempty
    BoolF x p -> mempty
    NotF x p -> f x
    LessThanF x y p -> (f x) <> (f y)
    IfF c x y p -> (f c) <> (f x) <> (f y)

instance htraversableExpr :: HTraversable ExprF where
  htraverse f = case _ of
    AddF x y p -> lift2 (\x' y' -> AddF x' y' p) (f x) (f y)
    MulF x y p -> lift2 (\x' y' -> MulF x' y' p) (f x) (f y)
    EqualF x y p -> lift2 (\x' y' -> EqualF x' y' p) (f x) (f y)
    ValF x p -> pure (ValF x p)
    BoolF x p -> pure (BoolF x p)
    NotF x p -> (\x' -> NotF x' p) <$> f x
    LessThanF x y p -> lift2 (\x' y' -> LessThanF x' y' p) (f x) (f y)
    IfF c x y p -> lift3 (\x' y' z' -> IfF x' y' z' p) (f c) (f x) (f y)
module Language.Rosa.Core.Fix where

-- | Fixed-point of a functor
newtype Fix f = Fix { unFix :: f (Fix f) }

-- | Catamorphism over a Fixpoint
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi (Fix x) = phi (fmap (cata phi) x)

-- | Anamorphism over a Fixpoint
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi x = Fix (fmap (ana psi) (psi x))

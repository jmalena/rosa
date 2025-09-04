{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Core.Annotation where

import Language.Rosa.Core.SourceSpan

-- | A metadata annotated value
newtype Ann ann a = Ann { unAnn :: (ann, a) }
  deriving (Eq, Show, Functor)

-- | A value with source span annotation
type Located a = Ann SrcSpan a

-- | Construct an annotated value
mkAnn :: ann -> a -> Ann ann a
mkAnn a x = Ann (a, x)

-- | Symbolic alias for 'mkAnn'
infixr 0 @:
(@:) :: ann -> a -> Ann ann a
(@:) = mkAnn

-- | Access the value of an annotated value
val :: Ann ann a -> a
val (Ann (_, x)) = x

class HasAnn a where
  type AnnType a
  -- | Access the annotation
  ann :: a -> AnnType a
  -- | Replace the annotation
  setAnn :: AnnType a -> a -> a
  -- | Modify the annotation in place
  mapAnn :: (AnnType a -> AnnType a) -> a -> a
  mapAnn f x = setAnn (f (ann x)) x

instance HasAnn (Ann ann a) where
  type AnnType (Ann ann a) = ann
  ann (Ann (a, _)) = a
  setAnn a' (Ann (_, x)) = Ann (a', x)

{-
liftAnn :: Monad m => Ann ann a -> m a
withAnn :: Monad m => (ann -> m ()) -> Ann ann a -> m a
-}

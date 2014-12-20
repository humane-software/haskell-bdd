{-# LANGUAGE ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module Test.Bdd.Internal (
  GivenStore(..)
 ,GivenWithTeardown
 ,StorableAsGivenWithTeardown(..)
 ) where

data GivenStore m = forall a. GivenStore (m a, a -> m ())

type GivenWithTeardown m = Monad m => [GivenStore m]

class StorableAsGivenWithTeardown m g where
  mkGiven :: g -> GivenWithTeardown m

instance StorableAsGivenWithTeardown m (m a) where
  mkGiven g = [GivenStore (g,const (return ()))]

instance StorableAsGivenWithTeardown m (m a, a -> m ()) where
  mkGiven = return . GivenStore

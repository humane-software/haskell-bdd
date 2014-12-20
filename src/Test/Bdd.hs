{-# LANGUAGE BangPatterns, ExistentialQuantification, FlexibleContexts, ImpredicativeTypes, Rank2Types, ScopedTypeVariables #-}
module Test.Bdd (
  (^?=)
 ,Given
 ,Then
 ,When
 ,andAfter_
 ,expectError_
 ,given_
 ,noError
 ,testThat
 ,then_
 ,when_
 ) where

import Control.Monad.Error.Class
import Test.Bdd.Internal
import Test.HUnit ((@?=))

type Given m a = Monad m => m a

type When m a = Monad m => m a

type Then m a = Monad m => a -> m ()

testThat :: GivenWithTeardown m
testThat = []

infixl 1 `given_`, `when_`
infixl 2 `andAfter_`, `then_`

given_ :: (StorableAsGivenWithTeardown m g,Monad m) => GivenWithTeardown m -> g -> GivenWithTeardown m
given_ gs g = gs ++ mkGiven g

andAfter_ :: m a -> (a -> m ()) -> (m a, a -> m ())
andAfter_ = (,)

when_ :: Monad m => GivenWithTeardown m -> When m b -> m b
when_ (GivenStore (g,t):gs) w = do
  v <- g
  res <- when_ gs w
  t v
  return res
when_ [] w = w

then_ :: When m a -> Then m a -> When m a
then_ !w t = w >>= (\res-> (t $! res) >> return res)

expectError_ :: MonadError e m => When m a -> Then m e -> When m ()
expectError_ w t = (w >> fail "expected exception was not thrown") `catchError` (\e->t e)

noError :: Then m a
noError = const (return ())

(^?=) :: (Eq a, Show a) => IO a -> a -> b -> IO ()
actualF ^?= expected = (\_->actualF >>= (@?= expected))
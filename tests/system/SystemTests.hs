module Main where

import qualified BddTest
import Control.Monad (void)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMain (hUnitTestToTests BddTest.tests)
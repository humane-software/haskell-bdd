{-# LANGUAGE FlexibleContexts,RankNTypes,ScopedTypeVariables #-}

module BddTest (tests) where

import Control.Applicative ((<$>))
import Control.Arrow ((>>>))
import Control.Monad (when)
import Control.Exception
import Control.Monad.Writer
import System.Directory
import System.Process (runCommand)
import Test.Bdd
import Test.Bdd.Internal
import Test.HUnit

tests = test [

  "the most rudimentary construct (doesn't test anything yet)" ~:
    let runSomeImaginaryIOAction :: IO ()
        runSomeImaginaryIOAction = return ()
    in TestCase $
      testThat
      `when_` runSomeImaginaryIOAction

 ,"assert something" ~: do
    testThat
      `when_` writeFile "/tmp/BddTestFile" "xxx"
      `then_` readFile "/tmp/BddTestFile" ^?= "xxx"
    -- we'll explain how to teardown later
    void (runCommand "rm -f /tmp/BddTestFile")

 ,"multiple thens are possible" ~: do
    testThat
      `when_` writeFile "/tmp/BddTestFile2" "xxx"
      `then_` readFile "/tmp/BddTestFile2" ^?= "xxx"
      `then_` (elem "BddTestFile2" <$> getDirectoryContents "/tmp/") ^?= True
    -- we'll explain how to teardown later
    void (runCommand "rm -f /tmp/BddTestFile2")

 ,"preconditions are possible as givens" ~:
    let noFile :: FilePath -> Given IO ()
        noFile f = doesFileExist f >>= \b-> when b (removeFile f)
    in do
      testThat
        `given_` noFile "/tmp/BddTestFile"
        `when_` writeFile "/tmp/BddTestFile" "xxx"
        `then_` readFile "/tmp/BddTestFile" ^?= "xxx"
      -- we'll explain how to teardown later
      void (runCommand "rm -f /tmp/BddTestFile")

 ,"given can have a teardown, it takes given's return value" ~:
    let directory :: FilePath -> Given IO FilePath
        directory f = removeDir f >> createDirectory f >> return f
        removeDir :: FilePath -> IO ()
        removeDir f = doesDirectoryExist f >>= \exists->
                      when exists (removeDirectoryRecursive f)
    in testThat
      `given_` directory "/tmp/BddTestDir" `andAfter_` removeDir
      `when_` writeFile "/tmp/BddTestDir/test" "xxx"
      `then_` readFile "/tmp/BddTestDir/test" ^?= "xxx"

 ,"order of execution" ~:
    execWriter (
      (testThat::GivenWithTeardown (Writer [String]))
      `given_` (tell ["given"]::Writer [String] ()) `andAfter_` const (tell ["teardown"])
      `when_` tell ["when"]
      `then_` const (tell ["then"])
     ) @?= ["given","when","then","teardown"]

 ,"can expect an error" ~:
    testThat
    `when_` readFile "/that_surely_not_exists"
    `expectError_` (show >>> (@?= "/that_surely_not_exists: openFile: does not exist (No such file or directory)"))

 ,"test will fail if expected exception not thrown" ~:
    (do
       testThat
         `when_` readFile "/etc/passwd"
         `expectError_` (show >>> (@?= "this will never be thrown"))
       fail "should fail but didn't"
     ) `catch` (\(e::SomeException)->assertBool "expectError did not fail" (show e /= "user error (should fail but didn't)"))
 ]

module Main where

import qualified LexerTest as LexerTest
import qualified SwimLanesTest as SwimLanesTest
import System.Exit
import Test.HUnit

--run a suite of tests
runSuite :: Test -> IO Counts
runSuite = runTestTT

--folt the counts of multiple tests
foldCount :: Counts -> Counts -> Counts
foldCount (Counts a1 b1 c1 d1) (Counts a2 b2 c2 d2) =
    (Counts (a1+a2) (b1+b2) (c1+c2) (d1+d2))

--fold tests together
foldTests :: [Test] -> IO Counts
foldTests testList = do
    countList <- mapM runSuite testList
    return $ foldl foldCount (Counts 0 0 0 0) countList

--run all of our test suites
main :: IO ()
main = do
    (Counts _ _ errors failures) <- foldTests [LexerTest.tests,
                                               SwimLanesTest.tests]

    --error handling
    let errs = errors + failures
    if errs > 0
        then die $ "There were " ++ (show errs) ++ " issues."
        else return ()

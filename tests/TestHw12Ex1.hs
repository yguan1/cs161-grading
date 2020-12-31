module TestHw12Ex1 where

import qualified Hw12Ex1
import System.Process
import System.IO


test_empty :: IO ()
test_empty = do
    putStrLn "Testing with empty input"
    callCommand "touch input1"
    callCommand "./enumerate < input1 || true"

test_one :: IO ()
test_one = do
    putStrLn "Testing with one line input"
    callCommand "echo Hello World! > input2"
    callCommand "./enumerate < input2 || true"

test_normal :: IO ()
test_normal = do
    putStrLn "Testing with normal input"
    writeFile "input3" $ unlines . map show $ [1..13]
    callCommand "./enumerate < input3 || true"

test_long :: IO ()
test_long = do
    putStrLn "Testing with long input (only head and tail will be shown)"
    writeFile "input4" $ unlines . map show $ [1..100]
    callCommand "./enumerate < input4 > output4"
    callCommand "head output4 && tail output4"

compileHomework :: IO ()
compileHomework = do
    callCommand "ghc Hw12Ex1.hs -main-is Hw12Ex1 -o enumerate"


main = do
    compileHomework
    test_empty
    test_one
    test_normal
    test_long


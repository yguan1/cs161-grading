module TestHw16Ex1 where


import Prelude hiding (head)
import Hw16Ex1 hiding (main)


testTrivial :: IO ()
testTrivial = do
    putStrLn "Testing with trivial input"
    putStrLn . render $ string ""

testDeep :: IO ()
testDeep = do
    putStrLn "Testing with a deep nesting"
    putStrLn . render .
        tag "lol" . 
            tag "lol" .
                tag "lol" .
                    tag "lol" .
                        tag "lol" .
                            tag "lol" .
                                tag "lol" .
                                    tag "lol" .
                                        tag "lol" .
                                            tag "lol" .
                                                tag "lol" $ string "kek"

testExample :: IO ()
testExample = do
    putStrLn "Testing with provided example"
    putStrLn . render $ html $ do
        head $ do
            title $ string "Hello, world!"
        body $ do
            h1 $ string "Greetings"
            p $ string "Hello, world!"





-- Run tests
--
main = do
    testTrivial
    testDeep
    testExample

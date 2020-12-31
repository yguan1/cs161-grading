{-# LANGUAGE TemplateHaskell #-}

module TestHw18Ex1 where

import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Char

import Test.QuickCheck
--import TH

import Hw18Ex1

greedy_ :: ReadP a -> ReadP [a]
greedy_ p = ((:) <$> p <*> greedy_ p) <++ pure []

compareResults :: (Show a, Eq a) => ReadP a -> String -> Property
compareResults p s =
  let
    studentResult = readP_to_S (greedy  p) s
    graderResult  = readP_to_S (greedy_ p) s
  in property $ 
    and ((`elem` graderResult) <$> studentResult) &&
    and ((`elem` studentResult) <$> graderResult)

-- Actual tests

prop_test1 = compareResults (string "a" +++ string "ab") "abaac"
prop_test2 = compareResults (string "a") "aaaaaaaaaaaaa"
prop_test3 = compareResults (string "b") "aaaaaaaaaaaaa"
prop_test4 = compareResults (string "ab") "aaaaaaaaaaaaa"
prop_test5 = compareResults (satisfy isDigit) "123123123"
prop_test6 = compareResults (satisfy isDigit) "12312a3123"

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

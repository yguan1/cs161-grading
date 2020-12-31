{-# LANGUAGE TemplateHaskell #-}

module TestHw17Ex2 where

import Hw17Ex2
import TH

import Test.QuickCheck
import Text.Printf

$(insertTypeNameNotDefined "ComplexInt" [d|
    data ComplexInt = ComplexInt Int Int
        deriving (Show, Eq)
        |])

$(insertTypeNotInstance ''Show [t|ComplexInt|] [d|
    instance Show ComplexInt
        |])

$(insertTypeNotInstance ''Read [t|ComplexInt|] [d|
    instance Read ComplexInt where
            readsPrec = undefined
            |])

$(insertTypeNotInstance ''Eq [t|ComplexInt|] [d|
    instance Eq ComplexInt where
        ComplexInt a b == ComplexInt a' b' = a' == a && b' == b
        |])

printTuple (ComplexInt r i) = printf "(%d,%d)" r i

instance Arbitrary ComplexInt where
    arbitrary = ComplexInt <$> a <*> b
        where a = getNonNegative <$> arbitrary
              b = getNonNegative <$> arbitrary

-- Specialize type
readComplex :: String -> ComplexInt
readComplex = read

-- Failure tests, parser should reject malformed input
prop_unbalancedParens1 = expectFailure $ readComplex "((1,1)" `seq` True
prop_unbalancedParens2 = expectFailure $ readComplex "(1,1))" `seq` True
prop_unbalancedParens3 = expectFailure $ readComplex "(1,1" `seq` True
prop_unbalancedParens4 = expectFailure $ readComplex "1,1)" `seq` True

prop_extraParens = expectFailure $ readComplex "((1,1))" `seq` True

prop_extraCharacters = expectFailure $ readComplex "(1,1)1" `seq` True

prop_extraComma = expectFailure $ readComplex "(1,,1)" `seq` True

prop_missingComma = expectFailure $ readComplex "(1 1)" `seq` True

prop_missingNumber1 = expectFailure $ readComplex "(1,)" `seq` True
prop_missingNumber2 = expectFailure $ readComplex "(,1)" `seq` True

prop_emptyInput = expectFailure $ readComplex "" `seq` True

prop_junk1 = expectFailure $ readComplex "oaeudtmebodig" `seq` True
prop_junk2 = expectFailure $ readComplex "1ethaudnotehus" `seq` True
prop_junk3 = expectFailure $ readComplex "(oetuhasnoeu3h,12nth)" `seq` True
prop_junk4 = expectFailure $ readComplex "uidhtcgdh1" `seq` True

prop_no_memes = expectFailure $ readComplex "kek" `seq` True

-- Actual tests
prop_identity1 c = readComplex (printTuple c) === c

prop_identity2 (NonNegative a) = readComplex (show a) === ComplexInt a 0

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

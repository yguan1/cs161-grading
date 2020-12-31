{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module TestHw5Ex1 where

import Hw5Ex1 hiding (main)

import TH
import Test.QuickCheck
import Language.Haskell.TH

-- Expected stuff
--
prop_pair_exists = property $ $(testTypeExists "Pair")
$(insertTypeNameNotDefined "Pair" [d|
    data Pair a b = Pair a b
        |])

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

$(insertTypeNotInstance ''Eq [t| Pair Integer Integer |] [d|
    instance (Eq a, Eq b) => Eq (Pair a b) where
        (Pair a b) == (Pair a' b') = a' == a && b' == b
        |])
$(insertTypeNotInstance ''Show [t| Pair Integer Integer |] [d|
    instance (Show a, Show b) => Show (Pair a b) where
        show (Pair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
        |])

$(insertTypeNotInstance ''Ord [t| Pair Integer Integer |] [d|
    instance (Ord a, Ord b) => Ord (Pair a b) where
        _ <= _ = undefined
        |])

-- Tests

prop_transitivity :: Pair Integer Integer -> Pair Integer Integer -> Pair Integer Integer -> Property
prop_transitivity a b c = prop_pair_exists .&&. (pred ==> res)
    where pred = a <= b && b <= c
          res = a <= c

prop_reflexivity :: Pair Integer Integer -> Property
prop_reflexivity a = prop_pair_exists .&&. ((a <= a) === True)

prop_antisymmetry :: Pair Integer Integer -> Pair Integer Integer -> Property
prop_antisymmetry a b = prop_pair_exists .&&. (pred ==> res)
    where pred = a <= b && b <= a
          res = a === b

-- Just checking if the definition is general
prop_non_integer :: Bool
prop_non_integer = $(testTypeIsInstance ''Ord [t| Pair String String |])

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

{-# LANGUAGE TemplateHaskell #-}

module TestHw10Ex1 where

import Hw10Ex1
import TH
import Test.QuickCheck
import Language.Haskell.TH

data Func a b = Func (a -> b)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Func a b) where
    arbitrary = Func <$> arbitrary

instance Show (Func a b) where
    show _ = "<function>"

instance Eq (Func a b) where
    _ == _ = False

-- Expected stuff
$(autoFail "liftAN" 2)

-- Tests
kek :: Applicative f => ([a] -> b) -> f [a] -> f b
kek = fmap

prop_lol :: Func [Integer] String -> Maybe [Integer] -> Property
prop_lol (Func f) a = kek f a === liftAN f a

prop_lel :: Func [Integer] Integer -> [[Integer]] -> Property
prop_lel (Func f) a = kek f a === liftAN f a

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

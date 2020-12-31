{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module TestHw9Ex1 where

import Hw9Ex1
import TH
import Test.QuickCheck
import Language.Haskell.TH

-- Expected stuff
$(insertTypeNameNotDefined "BinaryTree" [d|
    data BinaryTree a = EmptyTree
                      | Node a (BinaryTree a) (BinaryTree a)
        deriving (Show, Eq)
        |])

$(insertTypeNotInstance ''Show [t|BinaryTree Int|] [d|
    instance (Show a) => Show (BinaryTree a)
        |])

$(insertTypeNotInstance ''Eq [t|BinaryTree Int|] [d|
    instance (Eq a) => Eq (BinaryTree a) where
        EmptyTree == EmptyTree = True
        Node a l r == Node b l' r' = a == b && l == l' && r == r'
        _ == _ = False
        |])

$(insertTypeNotInstance ''Functor [t|BinaryTree|] [d|
    instance Functor BinaryTree where
        fmap _ _ = undefined
        |])

instance (Arbitrary a) => Arbitrary (BinaryTree a) where
    arbitrary = sized btree
        where btree 0 = return EmptyTree
              btree n = frequency [
                  (3, return EmptyTree), 
                  (7, Node <$> arbitrary <*> subtree <*> subtree)]
                  where subtree = btree $ n `div` 2

instance (CoArbitrary a) => CoArbitrary (BinaryTree a) where
    coarbitrary EmptyTree = variant 0
    coarbitrary (Node a l r) = variant 1 . coarbitrary a . coarbitrary l . coarbitrary r

newtype Func a b = Func (a -> b)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Func a b) where
    arbitrary = Func <$> arbitrary

instance Show (Func a b) where
    show _ = "<function>"

instance Eq (Func a b) where
    _ == _ = False

-- Functor laws
prop_identity :: (Show a, Eq a) => BinaryTree a -> Property
prop_identity bt = (id <$> bt) === bt

prop_composition :: BinaryTree Integer -> Func Integer String -> Func String Integer -> Property
prop_composition bt (Func f) (Func g) = (g . f <$> bt) === (fmap g . fmap f $ bt)

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

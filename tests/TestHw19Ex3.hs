{-# LANGUAGE TemplateHaskell #-}

module TestHw19Ex3 where

import Control.Monad.State.Lazy

import Test.QuickCheck hiding (label)
import TH

import Hw19Ex3 (addLabels)

$(insertTypeNameNotDefined "BinaryTree" [d|
    data BinaryTree a
            = Empty
            | Node (BinaryTree a) a (BinaryTree a)
            deriving (Show, Eq)
    |])

$(insertTypeNotInstance ''Eq [t|BinaryTree Integer|] [d|
    instance Eq a => Eq (BinaryTree a) where
        Empty == Empty = True
        Node l a r == Node l' a' r' = a == a' && l == l' && r == r'
        _ == _ = False
        |])

$(insertTypeNotInstance ''Foldable [t|BinaryTree|] [d|
    instance Foldable BinaryTree where
        foldMap _ Empty = mempty
        foldMap f (Node left a right) =
            foldMap f left <> f a <> foldMap f right
        |])

$(insertTypeNotInstance ''Functor [t|BinaryTree|] [d|
    instance Functor BinaryTree where
        fmap _ Empty = Empty
        fmap f (Node left a right) =
            Node (fmap f left) (f a) (fmap f right)
        |])

$(insertTypeNotInstance ''Traversable [t|BinaryTree|] [d|
    instance Traversable BinaryTree where
        traverse _ Empty = pure Empty
        traverse f (Node left a right) =
             Node <$> traverse f left <*> f a <*> traverse f right
        |])

$(insertTypeNotInstance ''Show [t|BinaryTree Int|] [d|
    instance (Show a) => Show (BinaryTree a)
        |])

instance (Arbitrary a) => Arbitrary (BinaryTree a) where
    arbitrary = sized btree
        where btree 0 = return Empty
              btree n = frequency [
                  (3, return Empty),
                  (7, Node <$> subtree <*> arbitrary <*> subtree)]
                  where subtree = btree $ n `div` 2

instance (CoArbitrary a) => CoArbitrary (BinaryTree a) where
    coarbitrary Empty = variant 0
    coarbitrary (Node a l r) = variant 1 . coarbitrary a . coarbitrary l . coarbitrary r

newtype Func a b = Func (a -> b)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Func a b) where
    arbitrary = Func <$> arbitrary

instance Show (Func a b) where
    show _ = "<function>"

instance Eq (Func a b) where
    _ == _ = False

label a = do 
    ix <- get
    modify (+1)
    return (ix,a)

addLabels' ta = evalState (traverse label ta) 1

$(autoFail "addLabels" 1)

prop_empty :: Property
prop_empty = addLabels ([] :: [Integer])  === []

prop_equal :: BinaryTree Integer -> Property
prop_equal bt = addLabels bt === addLabels' bt

prop_polymorphic :: [Integer] -> Property
prop_polymorphic list = addLabels list === addLabels' list

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

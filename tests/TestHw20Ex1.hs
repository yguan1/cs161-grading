{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module TestHw20Ex1 where

import TH
import Test.QuickCheck

import Hw20Ex1 hiding (main)

$(insertTypeNameNotDefined "MaybeT" [d|
    newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) } 
    |])

$(insertTypeNotInstance ''Functor [t|MaybeT []|] [d|
    instance Functor m => Functor (MaybeT m) where
        fmap = undefined
        |])

$(insertTypeNotInstance ''Show [t|MaybeT [] Integer|] [d|
    instance Show (m (Maybe a)) => Show (MaybeT m a) where
        show v = "MaybeT " ++ show (runMaybeT v)
        |])

instance (Arbitrary (m (Maybe a))) => Arbitrary (MaybeT m a) where
    arbitrary = MaybeT <$> arbitrary

instance (CoArbitrary (m (Maybe a))) => CoArbitrary (MaybeT m a) where
    coarbitrary = coarbitrary . runMaybeT

instance Show ((->) a b) where
    show _ = "<function>"

lift :: Functor m => m a -> MaybeT m a
lift v = MaybeT $ Just <$> v

-- Functor []
identity x = runMaybeT x === runMaybeT (id <$> x)
composition f g x = runMaybeT (fmap (g . f) x) === runMaybeT (fmap g (fmap f x))

prop_list_identity :: MaybeT [] Integer -> Property
prop_list_identity = identity

prop_list_composition :: (Integer -> Integer) -> (Integer -> Integer) -> MaybeT [] Integer -> Property
prop_list_composition = composition

-- Functor Maybe
prop_maybe_identity :: MaybeT Maybe String -> Property
prop_maybe_identity = identity

prop_maybe_composition :: (Integer -> String) -> (String -> Bool) -> MaybeT Maybe Integer -> Property
prop_maybe_composition = composition

-- Run tests
return []
runTests = $(quickCheckAll)
main = runTests

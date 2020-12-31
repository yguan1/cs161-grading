{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TestHw20Ex2 where

import TH
import Test.QuickCheck

import Hw20Ex2 hiding (main)

$(insertTypeNameNotDefined "MaybeT" [d|
    newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) } 
    |])

$(insertTypeNotInstance ''Functor [t|MaybeT []|] [d|
    instance Functor m => Functor (MaybeT m) where
        fmap = undefined
        |])

$(insertTypeNotInstance ''Applicative [t|MaybeT []|] [d|
    instance Monad m => Applicative (MaybeT m) where
        pure = undefined
        (<*>) = undefined
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
identity x = runMaybeT x === runMaybeT (pure id <*> x)
composition f g x = runMaybeT l === runMaybeT r
    where l = pure (.) <*> g <*> f <*> x
          r = g <*> (f <*> x)
homomorphism_list f x = runMaybeT l === runMaybeT r 
    where l = pure f <*> pure x
          r = pure (f x) :: MaybeT [] Integer

homomorphism_maybe f x = runMaybeT l === runMaybeT r 
    where l = pure f <*> pure x
          r = pure (f x) :: MaybeT Maybe Integer

interchange u y = runMaybeT l === runMaybeT r
    where l = u <*> pure y
          r = pure ($ y) <*> u

prop_list_identity :: MaybeT [] Integer -> Property
prop_list_identity = identity

prop_list_composition :: MaybeT [] (Integer -> Integer) -> MaybeT [] (Integer -> Integer) -> MaybeT [] Integer -> Property
prop_list_composition = composition

prop_list_homomorphism :: (Integer -> Integer) -> Integer -> Property
prop_list_homomorphism = homomorphism_list

prop_list_interchange :: MaybeT [] (String -> String) -> String -> Property
prop_list_interchange = interchange

-- Functor Maybe
prop_maybe_identity :: MaybeT Maybe String -> Property
prop_maybe_identity = identity

prop_maybe_composition :: MaybeT Maybe (Integer -> String) -> MaybeT Maybe (String -> Bool) -> MaybeT Maybe Integer -> Property
prop_maybe_composition = composition

prop_maybe_homomorphism :: (String -> Integer) -> String -> Property
prop_maybe_homomorphism = homomorphism_maybe

prop_maybe_interchange :: MaybeT Maybe (String -> Bool) -> String -> Property
prop_maybe_interchange = interchange

-- Run tests
return []
runTests = $(quickCheckAll)
main = runTests

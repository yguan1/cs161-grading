{-# LANGUAGE TemplateHaskell #-}

module TestHw4Ex1 where

import Hw4Ex1 hiding (main)

import TH
import Test.QuickCheck
import Language.Haskell.TH

import qualified Prelude (even)
import Prelude hiding (even)

-- Expected stuff
$(autoFail "even" 1)

-- Tests

-- Test against prelude even
prop_ref_even :: Integer -> Property
prop_ref_even n = even n === Prelude.even n

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

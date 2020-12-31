{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module TestHw5Ex3 where

import Hw5Ex3 hiding (main)

import TH
import Test.QuickCheck
import Language.Haskell.TH

import Prelude hiding (foldr)

-- Expected stuff
--
$(autoFail "foldr" 3)

-- Tests

prop_ref_foldr = foldr (++) "" ["Hello", "World"] === "HelloWorld"

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

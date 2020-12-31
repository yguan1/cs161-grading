{-# LANGUAGE TemplateHaskell #-}

module TestHw13Ex4 where

import Hw13Ex4 hiding (main)
import TH
import Test.QuickCheck
import Language.Haskell.TH

import Control.Applicative
import Data.List

-- Expected stuff
$(autoFail "altconcat" 1)

-- Tests
prop_empty1 = altconcat ([] :: [Maybe ()]) === Nothing
prop_empty2 = altconcat ([] :: [[()]]) === []

prop_concat :: [[Integer]] -> Property
prop_concat l = altconcat l === concat l

prop_non_commutative = altconcat [[1], [2]] /= altconcat [[2], [1]]

ref = foldl (<|>) empty
prop_maybe :: [Maybe ()] -> Property
prop_maybe l = ref l === altconcat l

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

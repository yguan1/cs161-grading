{-# LANGUAGE TemplateHaskell #-}

module TestHw1Ex2 where

import Hw1Ex2

import TH
import Test.QuickCheck
import Language.Haskell.TH


-- Auto recovery in case of missing implementations
$(autoFail "law_of_cosines" 3)

-- Tests
ref_LOC :: Double -> Double -> Double -> Double
ref_LOC a b angle = sqrt $ a ^ 2 + b ^ 2 - 2 * a * b * cos rad
    where rad = angle / 360 * 2 * pi

prop_ref_law_of_cosine :: Double -> Double -> Double -> Bool
prop_ref_law_of_cosine a b angle = abs diff < 0.005
    where diff = res1 - res2
          res1 = ref_LOC a b angle
          res2 = law_of_cosines a b angle

--- RUN TESTS
return []
runTests = $quickCheckAll
main = runTests

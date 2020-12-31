{-# LANGUAGE TemplateHaskell #-}

module TestHw3Ex1 where

import Hw3Ex1

import TH
import Test.QuickCheck
import Language.Haskell.TH

-- Expected stuff
$(autoFail "collatz" 1)
$(autoFail "collatz'" 1)

-- Tests

ref_collatz n | n == 1 = 1
              | even n = 1 + ref_collatz (n `div` 2)
              | otherwise = 1 + ref_collatz (3 * n + 1)


-- Test results equal
prop_collatz_base = all (==1) vals
    where vals = [collatz 1, collatz' 1]

prop_collatz_ref :: Positive Integer -> Bool
prop_collatz_ref a = all (==exp) vals
    where exp = ref_collatz n
          vals = [collatz n, collatz' n]
          n = getPositive a

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

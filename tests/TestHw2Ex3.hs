{-# LANGUAGE TemplateHaskell #-}

module TestHw2Ex3 where

import Hw2Ex3

import TH
import Test.QuickCheck
import Language.Haskell.TH
import qualified Prelude (filter)
import Prelude hiding (filter)

-- Expected stuff
$(autoFail "result" 0)
$(autoFail "result2" 0)
$(autoFail "result3" 0)
$(autoFail "divisibleBy" 2)
$(autoFail "allp" 2)
$(autoFail "filterAll" 2)

-- Tests

ref_divisibleBy d n = n `mod` d == 0
ref_allp preds v = all ($ v) preds
ref_filterAll preds = Prelude.filter (ref_allp preds)
ref_result = sum . take 100 . filterAll preds $ [0..]
    where preds = [ref_divisibleBy 2, ref_divisibleBy 3,
                   not . ref_divisibleBy 4, not . ref_divisibleBy 9]

-- Test results equal
prop_test_result = result == result2 && result2 == result3 
    && result == result3 && ref_result == result

prop_divisible_iff_mod0 :: NonZero (Small Integer) -> Small Integer -> Property
prop_divisible_iff_mod0 a b = classify expected "divisible" $ expected === actual
    where expected = n `mod` d == 0
          actual = divisibleBy d n
          n = getSmall b
          d = getSmall $ getNonZero a

prop_allp_base_case :: Integer -> Property
prop_allp_base_case x = allp [] x === True

prop_filterAll_base_case :: [Integer] -> Property
prop_filterAll_base_case lst = filterAll [] lst === lst

prop_filterAll_base_case2 = filterAll [error "haha"] [] === ([] :: [Integer])

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

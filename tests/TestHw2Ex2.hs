{-# LANGUAGE TemplateHaskell #-}

module TestHw2Ex2 where

import Hw2Ex2

import TH
import Test.QuickCheck
import Language.Haskell.TH
import qualified Prelude (product)
import Prelude hiding (product)

-- Expected stuff
$(autoFail "product" 1)

-- Tests
prop_product10 :: Property
prop_product10 = counterexample "product from 1 to 10 is incorrect" $ 
    product lst === Prelude.product lst
        where lst = [1..10]

-- Test edge case
prop_product_empty :: Property
prop_product_empty = counterexample "base case incorrect" $
    product [] === 1

prop_product_single :: Property
prop_product_single = product [42069] === 42069

prop_product_non_consecutive :: Property
prop_product_non_consecutive = product [3, 9] === 27

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

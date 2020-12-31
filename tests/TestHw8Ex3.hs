{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module TestHw8Ex3 where

import Hw8Ex3

import TH
import Test.QuickCheck
import Language.Haskell.TH

-- Expected stuff
--
lmao = (>>= id)

prop_ayy_lmao :: Maybe (Maybe Int) -> Property
prop_ayy_lmao f = lmao f === joinMaybe f


-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

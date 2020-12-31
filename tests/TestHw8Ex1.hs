{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module TestHw8Ex1 where

import Hw8Ex1
import TH
import Test.QuickCheck
import Language.Haskell.TH

instance Show (a -> b) where
  show f = "<function>"

-- Expected stuff
--
lel = map . fmap

prop_haha :: (Int -> Int) -> [Maybe Int] -> Property
prop_haha f l = lel f l === mapListMaybe f l

-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests

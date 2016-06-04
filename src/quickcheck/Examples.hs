{-# LANGUAGE TemplateHaskell #-}

module Quickcheck.Examples where

import Test.QuickCheck.All


runTests = $quickCheckAll


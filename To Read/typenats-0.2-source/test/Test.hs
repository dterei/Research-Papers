{-
 -
 -  Copyright 2005-2006, Robert Dockins.
 -
 -}

module Main where

import Test.HUnit

import TypeNats
import MkTypeNats
	
$(mkTypeNatTestCase 10)

main = runTestTT (TestList typeNatTests)

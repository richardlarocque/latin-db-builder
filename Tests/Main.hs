module Main where

import Test.HUnit

import Tests.PageTest

main = do
	runTestTT allTests

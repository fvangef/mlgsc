{-# LANGUAGE OverloadedStrings #-}
 
import Test.HUnit

import NewickParser
import NewickDumper


nw1 = "(A,B);"

test1 = TestCase (do 
            (Just nwin) <- parseNewick nw1
            nwout = toNewick nwin
            assertEqual "RW (A,B)" nw1 nwout)
            
tests = TestList [
		TestLabel "(A,B);" test1
		]

main = do
	runTestTT tests


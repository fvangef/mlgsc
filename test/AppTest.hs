module AppTest where

import System.Process
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import Data.String.Utils

build_dir = "./dist/build"
test_dir = "./test"


type ProgName = String
type TestName = String

runTestCase :: ProgName -> TestName -> String -> IO ExitCode
runTestCase pname tname args = do
	let prog = build_dir `combine` pname `combine` pname
	let ftname = join "_" [pname, tname]
	let obtained = test_dir `combine` (ftname ++ ".out")
	let expected = test_dir `combine` (ftname ++ ".exp")
	exitCode <- runProgram prog args obtained
	case exitCode of
		ExitFailure val -> exitWith exitCode
		ExitSuccess -> compareOutputs obtained expected

-- Allows to bind (as in >>=) tests together, propagating failure should it
-- happen.

thenTestCase :: ProgName -> TestName -> String -> ExitCode -> IO ExitCode
thenTestCase pname tname args exit = case exit of
	ExitFailure val -> return exit
	ExitSuccess	-> runTestCase pname tname args

-- Run program (1st arg), passes arg to it via 2nd arg (space-sparated
-- arguments), 3rd arg is name of output file. This assumes that the program to
-- test send output to stdout.

runProgram :: FilePath -> String -> FilePath -> IO ExitCode
runProgram prog args obt = do
		let progCmd = join " " [prog, args, ">", obt]
		let progProcess = shell progCmd
		(i,o,e,h) <- createProcess progProcess
		waitForProcess h
		

-- Compare obtained with expected outputs, using diff(1).
--
compareOutputs :: FilePath -> FilePath -> IO ExitCode
compareOutputs a b = do
		let diffCmd = join " " ["diff", "-sq", a, b]
		let diffProcess = shell diffCmd
		(i,o,e,h) <- createProcess diffProcess
		waitForProcess h

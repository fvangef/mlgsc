import System.Exit

import AppTest

prog_name = "mlgsc"

args_1 = "./data/firmicutes_gt1.aln ./test/firmicutes_gt1.mod"
args_2 = "-sx ./data/firmicutes_gt1.aln ./test/firmicutes_gt1.mod"
args_3 = "-x ./data/firmicute_Spo0A_byGenus.pep ./data/firm_Spo0A_nobal_noweight.mod"
args_4 = "-x ./data/firmicute_Spo0A_byGenus.pep ./data/firm_Spo0A_bal_noweight.mod"
args_5 = "-x ./data/firmicute_Spo0A_byGenus.pep ./data/firm_Spo0A_nobal_weight.mod"
args_6 = "-x ./data/firmicute_Spo0A_byGenus.pep ./data/firm_Spo0A_bal_weight.mod"

main :: IO ExitCode
main = do
	runTestCase prog_name "firmic_Spo0A" args_1
	>>= thenTestCase prog_name "firmic_Spo0A_x" args_2
	>>= thenTestCase prog_name "firmic2_Spo0A_x" args_3
	>>= thenTestCase prog_name "firmic2_Spo0A_xb" args_4
	>>= thenTestCase prog_name "firmic2_Spo0A_xw" args_5
	>>= thenTestCase prog_name "firmic2_Spo0A_xbw" args_6
	>>= exitWith

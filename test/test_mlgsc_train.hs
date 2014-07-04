import System.Exit

import AppTest


prog_name = "mlgsc_train"

args1 = "-B -w n ./data/firmicutes_gt1.aln ./data/firmicutes_gt1.nw"
args2 = "-B -w n ./data/firmicute_Spo0A_byGenus.msa ./data/firmicute_genera.nw"
args3 = "-w n ./data/firmicute_Spo0A_byGenus.msa ./data/firmicute_genera.nw"
args4 = "./data/firmicute_Spo0A_byGenus.msa ./data/firmicute_genera.nw"
args5 = "-w o ./data/firmicute_Spo0A_byGenus.msa ./data/firmicute_genera.nw"

main :: IO ExitCode
main = do
	runTestCase prog_name "test1" args1
    >>= thenTestCase prog_name "test2" args2
    >>= thenTestCase prog_name "test3" args3
    >>= thenTestCase prog_name "test4" args4
    >>= thenTestCase prog_name "test5" args5
    >>= exitWith

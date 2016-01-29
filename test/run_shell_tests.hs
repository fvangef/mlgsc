import System.Process
-- import System.Directory
-- import System.Exit
-- import System.FilePath
-- import System.IO

-- import Data.String.Utils

runTestProcess :: FilePath -> IO ProcessHandle
runTestProcess script =
    runProcess script [] (Just "./test") Nothing Nothing Nothing Nothing 

main :: IO ()
main = do
    h1 <- runTestProcess "./test_mlgsc.sh"
    waitForProcess h1
    h2 <- runTestProcess "./test_mlgsc_xval.sh"
    waitForProcess h2
    return ()

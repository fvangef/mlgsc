module TestFileUtils (removeIfExists) where

import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)

-- see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists-in-haskell
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

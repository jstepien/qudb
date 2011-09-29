#!/usr/bin/env runhaskell

\begin{code}

import System.Directory
import System.Process
import System.IO
import qualified Control.Exception as E
import Data.List
import Control.Monad

data Result = OK | Failure String deriving (Show)

main = do dir <- getCurrentDirectory
          inPlace <- doesFileExist $ dir ++ "/Test.lhs"
          if inPlace
            then runSQLTests >>= printResults >> runQCTests
            else do setCurrentDirectory $ dir ++ "/test"
                    main

filesEndingWith :: String -> IO [FilePath]
filesEndingWith suffix = fmap (filter (suffix `isSuffixOf`)) allFiles

allFiles = getCurrentDirectory >>= getDirectoryContents

runSQLTests = do files <- filesEndingWith ".sql"
                 results <- mapM runTest files
                 putStrLn ""
                 return $ zip files results

runTest fn = run `E.catch` handle
  where run = do code <- readFile fn
                 expectation <- sqlite3 code
                 result <- qudb code
                 if expectation == result
                   then putStr "." >> return OK
                   else diff expectation result >>= fail
        handle :: E.SomeException -> IO Result
        handle ex = fail $ show ex
        fail msg = putStr "F" >> return (Failure msg)

diff expectation result = do f1 <- writeToFile expectation "sqlite3.txt"
                             f2 <- writeToFile result "qudb.txt"
                             diffOutput <- runDiff f1 f2
                             removeFile f1
                             removeFile f2
                             return diffOutput
  where writeToFile str pattern = do tmpDir <- getTemporaryDirectory
                                     (fn, h) <- openTempFile tmpDir pattern
                                     hPutStr h str
                                     hClose h
                                     return fn
        runDiff f1 f2 = do (_, out, _) <- readProcessWithExitCode "colordiff" ["-u", f1, f2] ""
                           return out

sqlite3 = readProcess "sqlite3" []

qudbPath = "../dist/build/qudb/qudb"

qudb = readProcess qudbPath []

printResults results = do printSummary
                          mapM_ printDetails failures
  where failures = filter (\r -> case snd r of OK -> False;  _ -> True) results
        failCount = length failures
        printSummary = unless (null failures) $
                         putStrLn $ show failCount ++ " " ++ plural ++ " failed"
        plural = if failCount == 1 then "test" else "tests"
        printDetails (test, (Failure err)) =
          putStrLn $ "\n" ++ indent (test : lines err) ++ "\n"
        indent lines = "  " ++ intercalate "\n  " lines

runQCTests = do files <- filesEndingWith ".hs"
                setCurrentDirectory ".."
                mapM_ run files
                setCurrentDirectory "test"
  where run file = do output <- readProcess "runhaskell" ["test/" ++ file] ""
                      putStr output

\end{code}

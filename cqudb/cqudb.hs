{-# LANGUAGE ScopedTypeVariables #-}

import Database.QUDB
import System.Environment
import System.Directory
import System.IO
import System.IO.Error
import qualified Control.Exception as E

main = do
  args <- getArgs
  if 1 /= length args
    then usage
    else (prepareDB . head) args >>= repl

usage = do
  name <- getProgName
  putStrLn $ "Usage: " ++ name ++ " filename"

prepareDB file = do
  gotFile <- doesFileExist file
  if gotFile
    then loadDB file
    else initDB file

repl db = loop `E.catch` handleIOExc `E.catch` handleOtherExc
  where loop = do putStr "> "
                  hFlush stdout
                  line <- getLine
                  if 0 < length line
                    then query db line >>= printResults >> loop
                    else loop
        handleIOExc = \(e :: E.IOException) -> if isEOFError e
                                                 then putStrLn ""
                                                 else ioError e
        handleOtherExc = \(e :: E.SomeException) -> print e >> repl db

printResults [[]] = return ()
printResults xs = mapM_ putStrLn . map columnify $ xs
  where columnify [] = ""
        columnify [x] = showValue x
        columnify (x:xs) = showValue x ++ "|" ++ columnify xs
        showValue (IntValue x) = show x
        showValue (StringValue x) = x

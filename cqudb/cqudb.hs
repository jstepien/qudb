{-# LANGUAGE ScopedTypeVariables #-}

import Database.QUDB
import System.Environment
import System.Directory
import System.IO
import System.IO.Error
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C (hGetContents, writeFile)

main = do
  args <- getArgs
  if length args > 1
    then usage
    else do
      let filename = if null args then Nothing else Just (head args)
      db <- prepareDB filename
      terminal <- hIsTerminalDevice stdin -- Not portable: GHC/Hugs only!
      db' <- if terminal
               then repl db
               else noninteractive db
      case filename of
        (Just name) -> C.writeFile name $ dumpDB db'
        _ -> return ()

usage = do
  name <- getProgName
  putStrLn $ "Usage: " ++ name ++ " [filename]"

prepareDB Nothing = return initDB
prepareDB (Just file) = do
  gotFile <- doesFileExist file
  if gotFile
    then do
      handle <- openFile file ReadMode
      dump <- C.hGetContents handle
      hClose handle
      return $ loadDB dump
    else return $ initDB

repl db = loop `E.catch` handleIOExc `E.catch` handleOtherExc
  where loop = do putStr "> "
                  hFlush stdout
                  line <- getLine
                  if 0 < length line
                    then let result = query db line in
                         case result of
                           Just (db', res) -> printResults res >> repl db'
                           Nothing -> putStrLn "query returned Nothing :(" >> loop
                    else loop
        handleIOExc = \(e :: E.IOException) -> if isEOFError e
                                                 then return db
                                                 else ioError e
        handleOtherExc = \(e :: E.SomeException) -> print e >> repl db

printResults xs = mapM_ putStrLn . map columnify $ xs
  where columnify [] = ""
        columnify [x] = showValue x
        columnify (x:xs) = showValue x ++ "|" ++ columnify xs
        showValue (IntValue x) = show x
        showValue (StringValue x) = x

noninteractive db = loop `E.catch` handleIOExc
  where loop = do line <- getLine
                  if 0 < length line
                    then let result = query db line in
                         case result of
                           Just (db', res) -> printResults res >> noninteractive db'
                           Nothing -> loop
                    else loop
        handleIOExc = \(e :: E.IOException) -> if isEOFError e
                                                 then return db
                                                 else ioError e

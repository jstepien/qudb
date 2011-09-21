import Database.QUDB
import System.Environment
import System.Directory
import System.IO
import System.IO.Error
import Data.Maybe (isJust)
import Control.Monad (when)
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
      when (isJust filename && db /= db') $
        let (Just name) = filename in C.writeFile name $ dumpDB db'

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

repl db = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  if eof
    then return db
    else do
      line <- getLine
      db' <- if null line
               then return db
               else (case query db line of
                            Just (db', res) -> printResults res >> return db'
                            Nothing -> return db
                    ) `E.catch` handler db
      repl db'
  where handler :: DB -> E.SomeException -> IO DB
        handler db e = print e >> return db

noninteractive db = do
  eof <- isEOF
  if eof
    then return db
    else do
      line <- getLine
      db' <- if null line
               then return db
               else case query db line of
                 Just (db', res) -> printResults res >> return db'
                 Nothing -> return db
      noninteractive db'

printResults xs = mapM_ putStrLn . map columnify $ xs
  where columnify [] = ""
        columnify [x] = showValue x
        columnify (x:xs) = showValue x ++ "|" ++ columnify xs
        showValue (IntValue x) = show x
        showValue (StringValue x) = x

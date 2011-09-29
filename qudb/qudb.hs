import qualified Database.QUDB as QUDB
import System.Environment
import System.Directory
import System.IO
import Data.Maybe (isJust)
import Control.Monad (when)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C (hGetContents, writeFile)

main :: IO ()
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
        let (Just name) = filename in C.writeFile name $ QUDB.dump db'

usage :: IO ()
usage = do
  name <- getProgName
  putStrLn $ "Usage: " ++ name ++ " [filename]"

prepareDB :: Maybe FilePath -> IO QUDB.DB
prepareDB Nothing = return QUDB.new
prepareDB (Just file) = do
  gotFile <- doesFileExist file
  if gotFile
    then do
      handle <- openFile file ReadMode
      dump <- C.hGetContents handle
      hClose handle
      return $ QUDB.load dump
    else return QUDB.new

repl :: QUDB.DB -> IO QUDB.DB
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
               else (case QUDB.query db line of
                            Right (db', res) -> printResults res >> return db'
                            Left msg -> putStrLn msg >> return db
                    ) `E.catch` handler db
      repl db'
  where handler :: QUDB.DB -> E.SomeException -> IO QUDB.DB
        handler db e = print e >> return db

noninteractive :: QUDB.DB -> IO QUDB.DB
noninteractive db = do
  eof <- isEOF
  if eof
    then return db
    else do
      line <- getLine
      db' <- if null line
               then return db
               else case QUDB.query db line of
                 Right (db', res) -> printResults res >> return db'
                 Left msg -> putStrLn msg >> return db
      noninteractive db'

printResults :: [[QUDB.Value]] -> IO ()
printResults xs = mapM_ putStrLn . map columnify $ xs
  where columnify [] = ""
        columnify [x] = showValue x
        columnify (x:xs) = showValue x ++ "|" ++ columnify xs
        showValue (QUDB.IntValue x) = show x
        showValue (QUDB.StringValue x) = x

import Database.QUDB
import System.Environment
import System.Directory
import System.IO

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

repl db = do
  putStr "> "
  hFlush stdout
  line <- getLine
  result <- query db line
  printResults result
  repl db

printResults = print

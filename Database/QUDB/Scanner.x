{
module Database.QUDB.Scanner (
    scan,
    Token(
        Symb, Str, Int, LParen, RParen, Comma, Select, Insert, Delete, From,
        Into, Asterisk, Values, Create, Table, Drop
        )
    ) where
}

%wrapper "basic"

$digit  = 0-9
$alpha  = [a-zA-Z]
$quotable = $printable # \'

tokens :-

  $white+                     ;
  "--".*                      ;
  \' $quotable* \'            { \s -> Str $ init $ tail s }
  \-?$digit+                  { \s -> Int (read s) }
  \(                          { \s -> LParen }
  \)                          { \s -> RParen }
  \,                          { \s -> Comma }
  "select"                    { \s -> Select }
  "insert"                    { \s -> Insert }
  "delete"                    { \s -> Delete }
  "from"                      { \s -> From }
  "into"                      { \s -> Into }
  "values"                    { \s -> Values }
  "create"                    { \s -> Create }
  "table"                     { \s -> Table }
  "drop"                      { \s -> Drop }
  \*                          { \s -> Asterisk }
  $alpha [$alpha $digit \_]*  { \s -> Symb s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Symb String |
  Str String  |
  Int Int     |
  LParen      |
  RParen      |
  Comma       |
  Select      |
  Insert      |
  Delete      |
  From        |
  Into        |
  Values      |
  Create      |
  Table       |
  Drop        |
  Asterisk
  deriving (Eq,Show)

scan = alexScanTokens
}

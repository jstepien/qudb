{
module Database.QUDB.Scanner (
    scan,
    Token(
        Symb, Str, Int, LParen, RParen, Comma, Select, Insert, From, Into,
        Asterisk, Values
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
  "from"                      { \s -> From }
  "into"                      { \s -> Into }
  "values"                    { \s -> Values }
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
  From        |
  Into        |
  Values      |
  Asterisk
  deriving (Eq,Show)

scan = alexScanTokens
}

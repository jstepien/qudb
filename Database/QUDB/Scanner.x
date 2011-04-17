{
module Database.QUDB.Scanner where
}

%wrapper "basic"

$digit  = 0-9
$alpha  = [a-zA-Z]
$quotable = $printable # \'

tokens :-

  $white+                     ;
  "--".*                      ;
  \' $quotable* \'            { \s -> Str s }
  \-?$digit+                  { \s -> Int (read s) }
  \(                          { \s -> LParen }
  \)                          { \s -> RParen }
  \,                          { \s -> Comma }
  $alpha [$alpha $digit \_]*  { \s -> Symb s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Quote       |
  Symb String |
  Str String  |
  Int Int     |
  LParen      |
  RParen      |
  Comma
  deriving (Eq,Show)

scan = alexScanTokens
}

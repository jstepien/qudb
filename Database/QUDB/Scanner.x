{
module Database.QUDB.Scanner (
    scan,
    Token(
        Symb, Str, Int, LParen, RParen, Comma, Select, Insert, Delete, Update,
        From, Into, Asterisk, Values, Create, Table, Drop, Equals, Where,
        Greater, Lesser, Set, And, Or, OrderBy, Asc, Desc, Limit, Semicolon
        )
    ) where
}

%wrapper "basic"

$digit  = 0-9
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$symbolic = $printable # [$special$white]
$quotable = $printable # \'

tokens :-

  $white+                     ;
  "--".*                      ;
  \' $quotable* \'            { \s -> Str $ init $ tail s }
  \-?$digit+                  { \s -> Int (read s) }
  \(                          { \s -> LParen }
  \)                          { \s -> RParen }
  \,                          { \s -> Comma }
  \=                          { \s -> Equals }
  \>                          { \s -> Greater }
  \<                          { \s -> Lesser }
  \;                          { \s -> Semicolon }
  "select"                    { \s -> Select }
  "insert"                    { \s -> Insert }
  "delete"                    { \s -> Delete }
  "update"                    { \s -> Update }
  "from"                      { \s -> From }
  "into"                      { \s -> Into }
  "values"                    { \s -> Values }
  "create"                    { \s -> Create }
  "table"                     { \s -> Table }
  "drop"                      { \s -> Drop }
  "where"                     { \s -> Where }
  "set"                       { \s -> Set }
  "and"                       { \s -> And }
  "or"                        { \s -> Or}
  "asc"                       { \s -> Asc}
  "desc"                      { \s -> Desc}
  "limit"                     { \s -> Limit }
  "order by"                  { \s -> OrderBy}
  \*                          { \s -> Asterisk }
  $symbolic+                  { \s -> Symb s }

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
  Update      |
  From        |
  Into        |
  Values      |
  Create      |
  Table       |
  Drop        |
  Where       |
  Equals      |
  Lesser      |
  Greater     |
  Set         |
  And         |
  Or          |
  OrderBy     |
  Asc         |
  Desc        |
  Limit       |
  Semicolon   |
  Asterisk
  deriving (Eq,Show)

scan = alexScanTokens
}

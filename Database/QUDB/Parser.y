{
module Database.QUDB.Parser (parse) where
import Database.QUDB.Scanner
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
      symb { Symb $$ }
      '('  { LParen }
      ')'  { RParen }

%%

Program : Lists               { $1 }

Lists : Lists List            { $1 ++ [$2] }
      | {- empty -}           { [] }

List : '(' SymbolsOrLists ')' { List $2 }

SymbolsOrLists : SymbolsOrLists symb  { $1 ++ [Symbol $2] }
               | SymbolsOrLists List  { $1 ++ [$2] }
               | {- empty -}          { [] }

{
parseError :: [Token] -> a
parseError (t:_) = error ("Parse error at " ++ show t)

data Elem =
  Symbol String |
  List [Elem]
  deriving (Eq,Show)

parse = parseTokens . scan
}

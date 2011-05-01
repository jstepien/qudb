{
module Database.QUDB.Parser (parse) where
import Database.QUDB.Scanner
import Database.QUDB.EntityTypes (Value(IntValue, StringValue))
import qualified Database.QUDB.Query as Q
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
      str    { Str $$ }
      int    { Int $$ }
      symb   { Symb $$ }
      '*'    { Asterisk }
      '('    { LParen }
      ')'    { RParen }
      ','    { Comma }
      select { Select }
      insert { Insert }
      from   { From }
      into   { Into }
      values { Values }
%%

Query : SelectQuery { $1 }
      | InsertQuery { $1 }

SelectQuery : select Columns from Table { Q.Select $4 }

InsertQuery: insert into Table values '(' Values ')' { Q.Insert $3 $6 }

Values : Value OtherValues { $1 : $2 }

OtherValues : ',' Value OtherValues { $2 : $3 }
            | {- empty -} { [] }

Value : str { StringValue $1 }
      | int { IntValue $1 }

Table : symb { $1 }

Columns : '*' { }

{
parseError :: [Token] -> a
parseError (t:_) = error ("Parse error at " ++ show t)

parse = parseTokens . scan
}

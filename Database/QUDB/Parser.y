{
module Database.QUDB.Parser (parse) where
import Database.QUDB.Scanner
import qualified Database.QUDB.EntityTypes as T
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
      table  { Table }
      create { Create }
      drop   { Drop }
%%

Query : SelectQuery { $1 }
      | InsertQuery { $1 }
      | CreateTableQuery { $1 }
      | DropTableQuery { $1 }

SelectQuery : select '*' from Table { Q.SelectAll : [Q.From $4] }
            | select Columns from Table { Q.Select $2 : [Q.From $4] }

InsertQuery: insert into Table values '(' Values ')' { Q.Insert $6 : [Q.From $3] }

CreateTableQuery : create table Table '(' ColumnsDefs ')' { [Q.CreateTable $3 $5] }

DropTableQuery : drop table Table { [Q.DropTable $3] }

Values : Value OtherValues { $1 : $2 }

OtherValues : ',' Value OtherValues { $2 : $3 }
            | {- empty -} { [] }

Value : str { T.StringValue $1 }
      | int { T.IntValue $1 }

Table : symb { $1 }

Columns : ColumnName OtherColumnNames { $1 : $2 }

OtherColumnNames: ',' ColumnName OtherColumnNames { $2 : $3 }
                | {- empty -} { [] }

ColumnName : symb { $1 }

ColumnsDefs : ColumnDef OtherColumnsDefs { $1 : $2 }

OtherColumnsDefs : ',' ColumnDef OtherColumnsDefs { $2 : $3 }
                 | {- empty -} { [] }

ColumnDef : symb symb { case $2 of
                          "int"     -> ($1, T.Int)
                          "string"  -> ($1, T.String)
                          otherwise -> error $ "No such type: " ++ $2 }

{
parseError :: [Token] -> a
parseError (t:_) = error ("Parse error at " ++ show t)

parse = parseTokens . scan
}

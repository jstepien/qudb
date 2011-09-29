{
module Database.QUDB.Parser (parse) where
import Database.QUDB.Scanner
import qualified Control.Monad.Error as E
import qualified Database.QUDB.EntityTypes as T
import qualified Database.QUDB.Query as Q
}

%name parseTokens
%tokentype { Token }
%error { parseError }
%monad { ParserMonad }

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
      delete { Delete }
      update { Update }
      from   { From }
      into   { Into }
      values { Values }
      table  { Table }
      create { Create }
      drop   { Drop }
      where  { Where }
      set    { Set }
      and    { And }
      or     { Or }
      asc    { Asc }
      desc   { Desc }
      limit  { Limit }
      '='    { Equals }
      '>'    { Greater }
      '<'    { Lesser }
      ';'    { Semicolon }
      orderBy { OrderBy }
%%

Query : QueryWithoutSemicolon ';' { $1 }

QueryWithoutSemicolon : SelectQuery { return $1 }
                      | InsertQuery { return $1 }
                      | DeleteQuery { return $1 }
                      | UpdateQuery { return $1 }
                      | CreateTableQuery { $1 }
                      | DropTableQuery { return $1 }

SelectQuery : select '*' from Table Clauses     { Q.SelectAll $4 : $5 }
            | select Columns from Table Clauses { Q.Select $4 $2 : $5 }

InsertQuery: insert into Table values '(' Values ')' { [Q.Insert $3 $6] }

DeleteQuery : delete from Table OptWhereClause { Q.Delete $3 : $4 }

UpdateQuery : update Table set UpdatedValues OptWhereClause { Q.Update $2 $4 : $5 }

CreateTableQuery : create table Table '(' ColumnsDefs ')' { $5 >>= \x -> return [Q.CreateTable $3 x] }

DropTableQuery : drop table Table { [Q.DropTable $3] }

Values : Value OtherValues { $1 : $2 }

OtherValues : ',' Value OtherValues { $2 : $3 }
            | {- empty -} { [] }

Value : str { T.StringValue $1 }
      | int { T.IntValue $1 }

UpdatedValues : UpdatedValue OptUpdatedValues { $1 : $2 }

UpdatedValue : ColumnName '=' Value { ($1, $3) }

OptUpdatedValues : ',' UpdatedValue OptUpdatedValues { $2 : $3 }
                 | {- empty -}                       { [] }

Table : symb { $1 }

Columns : ColumnName OtherColumnNames { $1 : $2 }

OtherColumnNames: ',' ColumnName OtherColumnNames { $2 : $3 }
                | {- empty -} { [] }

ColumnName : symb { $1 }

ColumnsDefs : ColumnDef OtherColumnsDefs { do x <- $1; y <- $2;
                                              return (x:y) }


OtherColumnsDefs : ',' ColumnDef OtherColumnsDefs { do x <- $2; y <- $3;
                                                       return (x:y) }
                 | {- empty -} { return [] }

ColumnDef : symb symb { case $2 of
                          "int"     -> return ($1, T.Int)
                          "string"  -> return ($1, T.String)
                          otherwise -> E.throwError $ NoSuchType $2 }

Clauses : Clause Clauses { $1 : $2 }
        | {- empty -}    { [] }

Clause : WhereClause { $1 }
       | OrderByClause { $1 }
       | LimitClause { $1 }

OrderByClause : orderBy OrderSpecs { Q.OrderBy $2 }

OrderSpecs : OrderSpec OptOrderSpecs { $1 : $2 }

OptOrderSpecs : ',' OrderSpec OptOrderSpecs { $2 : $3 }
              | {- empty -}                 { [] }

OrderSpec : ColumnName asc   { ($1, Q.Ascending) }
          | ColumnName desc  { ($1, Q.Descending) }
          | ColumnName       { ($1, Q.Ascending) }

OptWhereClause : WhereClause { [$1] }
               | {- empty -} { [] }

LimitClause : limit int { Q.Limit $2 }

WhereClause : where Predicates { Q.Where $2 }

Predicates : Predicate and PredicateConj { Q.AndConditions ($1:$3) }
           | Predicate or  PredicateDisj { Q.OrConditions ($1:$3) }
           | Predicate { $1 }

PredicateConj : Predicate and PredicateConj { $1 : $3 }
              | Predicate { [$1] }

PredicateDisj : Predicate or  PredicateDisj { $1 : $3 }
              | Predicate { [$1] }

Predicate : ColumnName '=' Value { Q.Condition $1 (== $3) }
          | ColumnName '<' Value { Q.Condition $1 (< $3) }
          | ColumnName '>' Value { Q.Condition $1 (> $3) }

{
type ParserMonad = Either ParsingError
data ParsingError = ParsingFailedAtToken Token
                  | NoSuchType String

instance E.Error ParsingError

instance Show ParsingError where
  show (ParsingFailedAtToken t) = "Parsing failed at " ++ show t
  show (NoSuchType t) = "No such type: " ++ t

parseError :: [Token] -> ParserMonad a
parseError (t:_) = E.throwError $ ParsingFailedAtToken t

parse :: String -> Either String [Q.Query]
parse input = case parseTokens $ scan input of
                Left err -> Left $ show err
                Right (Left err) -> Left $ show err
                Right (Right queries) -> Right queries
}

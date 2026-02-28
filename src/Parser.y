{
module Parser where
import Ast
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    let     { TLet }
    in      { TIn }
    if      { TIf }
    iszero  { TIsZero }
    then    { TThen }
    else    { TElse }
    fix     { TFix }
    lambda  { TLambda }
    int     { TInt $$ }
    var     { TVar $$ }
    '='     { TEq }
    '+'     { TPlus }
    '-'     { TMinus }
    '*'     { TTimes }
    '('     { TLParen }
    ')'     { TRParen }
    '.'     { TDot }
    '\\'    { TBackslash }

%right in
%left '+' '-'
%left '*'

%%

Term :: { Term }
Term
    : let var '=' Term in Term { TLet $2 $4 $6 }
    | lambda var '.' Term { TLambda $2 $4 }
    | '\\' var '.' Term { TLambda $2 $4 }
    | fix Term { TFix $2 }
    | if Var iszero Term then Term else Term { TIf $3 $5 $7 }
    | ArithmeticTerm { $1 }

ArithmeticTerm :: { Term }
ArithmeticTerm
    : ArithmeticTerm '+' ArithmeticTerm { TPlus $1 $3 }
    | ArithmeticTerm '-' ArithmeticTerm { TMinus $1 $3 }
    | ArithmeticTerm '*' ArithmeticTerm { TTimes $1 $3 }
    | ApplicationTerm { $1 }

ApplicationTerm :: { Term }
ApplicationTerm
    : ApplicationTerm ApplicationTerm { TApp $1 $2 }
    | AtomicTerm { $1 }

AtomicTerm :: { Term }
AtomicTerm
    : int { TInt $1 }
    | var { TVar $1 }
    | '(' Term ')' { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
    = TLet
    | TIn
    | TIf
    | TIsZero
    | TThen
    | TElse
    | TFix
    | TLambda
    | TInt Int
    | TVar String
    | TEq
    | TPlus
    | TMinus
    | TTimes
    | TLParen
    | TRParen
    | TDot
    | TBackslash
 deriving Show
}

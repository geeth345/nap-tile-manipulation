{ 
module NapGrammar where 
import NapTokens
}

%name parseCalc 
%tokentype { NapToken } 
%error { parseError }
%token 
    rotate { TokenRotate _ }
    hstick { TokenHStick _ }
    vstick { TokenVStick _ }
    size { TokenSize _ }
    Hrepeat { TokenHRepeat _ }
    Vrepeat { TokenVRepeat _ }
    Hreflect { TokenHReflect _ }
    Vreflect { TokenVReflect _ }
    blank { TokenBlank _ }
    full { TokenFull _ }
    scale { TokenScale _ }
    negate { TokenNegate _ }
    subtile { TokenSubtile _ }
    square { TokenSquare _ }
    Tand { TokenAnd _ }
    Tor { TokenOr _ }
    Tequal { TokenTileEqual _ }
    change { TokenChange _ }
    replace { TokenReplace _ }
    inspect { TokenInspect _ }
    create { TokenCreate _ }
    import { TokenImport _ }

    and { TokenAndBool _ }
    or { TokenOrBool _ }
    not { TokenNotBool _ }

    '||'  { TokenOrSugar _ }
    '&&'  { TokenAndSugar _ }
    '!'   { TokenNotSugar _}
    'T|'  {TokenTokOrSugar _ }
    'T&'   { TokenTokAndSugar _}
    'T!'   { TokenTokNotSugar _}

    '=='    { TokenEq _ }
    '='    { TokenAssign _ }
    '-'   { TokenMinus _ }
    '*'   { TokenTimes _ }
    idiv  { TokenDiv _ }
    '>'   { TokenGreaterThan _ }
    '<'    { TokenLessThan _ }
    '+'    { TokenPlus _ }

    fst    { TokenFst _ }
    snd    { TokenSnd _ }   
    ','    { TokenComma _ }

    while { TokenWhile _ }
    return { TokenReturn _ }
    for { TokenFor _ }
    do { TokenDo _ }
    to { TokenTo _ }
    continue { TokenContinue _ }

    NpTile { TokenTypeTile _ }
    NpCell { TokenTypeCell _ }
    Bool   { TokenTypeBool _ } 
    Int    { TokenTypeInt _ } 
    Unit   { TokenTypeUnit _ }
    arr    { TokenTypeArr _ } 
    int    { TokenInt _ $$ } 
    true   { TokenTrue _ }
    false  { TokenFalse _ }

    if     { TokenIf _ }
    then   { TokenThen _ }
    else   { TokenElse _ }


    let    { TokenLet _ }
    ':'    { TokenHasType _ }
    ';'    { TokenSemiColon _ }

    '['    { TokenLSquare _ }
    ']'    { TokenRSquare _ }
    '{'    { TokenLCurly _ }
    '}'    { TokenRCurly _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ } 
    lam    { TokenLambda _ }
    var    { TokenVar _ $$ }
    apply { TokenApply _ }
    list { TokenList _ }
    length { TokenLength _ }
    sum   { TokenListSum _ }
    prod { TokenListProd _ }
    hstickall { TokenHStickAll _ }
    vstickall { TokenVStickAll _ }
    append { TokenAppend _ }
    remove { TokenRemove _ }
    modify { TokenModify _ }
    fun { TokenFun _ }
    apply_fun { TokenApplyFun _ }
    map { TokenMap _ }


%nonassoc "="
%nonassoc int true false var '(' ')' '[' ']' '{' '}'
%left create
%left change
%left static
%left alter
%left local
%left Tand
%left Tor
%left Tequal
%left while
%left and
%left or
%left not
%left do
%left to 
%left fun 
%left apply_fun
%left map
%left square
%left replace
%left size
%left Hrepeat 
%left Vrepeat
%left Hreflect
%left Vreflect
%left blank 
%left full
%left scale 
%left negate
%left return
%left subtile 
%left inspect
%left rotate
%left hstick 
%left vstick
%left import
%left sum
%left prod
%left hstickall
%left vstickall
%left append
%left remove
%left modify
%left arr
%right let
%nonassoc if
%nonassoc then
%nonassoc else
%left ';'
%left fst snd
%nonassoc '<' '>'
%left '||'  'T|' 
%left '&&' 'T&'
%left '!' 'T!'
%left '-'
%left '+'
%left '*'
%left idiv
%left ','

%left lam
%left apply
%left NEG


%% 
Exp : rotate Exp Exp                            { TmRotate $2 $3 }
    | hstick Exp Exp                            { TmHStick $2 $3 }
    | vstick Exp Exp                            { TmVStick $2 $3 }
    | size Exp                                  { TmSize $2 }
    | Hrepeat Exp Exp                           { TmHrepeat $2 $3 }
    | Vrepeat Exp Exp                           { TmVrepeat $2 $3 }
    | Hreflect Exp                              { TmHreflect $2 }
    | Vreflect Exp                              { TmVreflect $2 }
    | blank Exp                                 { TmBlank $2 }
    | full Exp                                  { TmFull $2 }
    | scale Exp Exp                             { TmScale $2 $3 }
    | negate Exp                                { TmNegate $2 }
    | subtile Exp Exp Exp Exp                      { TmSubtile $2 $3 $4 $5 }
    | square Exp                                { TmSquare $2 }
    | Tequal Exp Exp                            { TmTileEqual $2 $3 }
    | Tand Exp Exp                               { TmAnd $2 $3 }
    | Tor Exp Exp                                { TmOr $2 $3 }
    | Exp 'T|' Exp                               { TmOr $1 $3 }
    | Exp 'T&' Exp                               { TmAnd $1 $3 }
    | 'T!' Exp                               { TmNegate $2  }
    | change Exp Exp Exp Exp                         { TmChange $2 $3 $4 $5}
    | inspect Exp Exp Exp                            { TmInspect $2 $3 $4}
    | replace Exp Exp Exp Exp                      { TmReplace $2 $3 $4 $5}
    | create Exp                            { TmCreate $2 }


    | import var                                { TmImport $2 }

    | and Exp  Exp                               { TmAndBool $2 $3 }
    | or Exp Exp                                { TmOrBool $2 $3 }
    | not Exp                                   { TmNotBool $2 }
    | Exp '&&' Exp                               { TmAndBool $1 $3 }
    | Exp '||' Exp                               { TmOrBool $1 $3 }
    | '!' Exp                                   { TmNotBool $2 }

    | Exp '<' Exp                               { TmCompare $1 $3 } 
    | Exp '-' Exp                               { TmSub $1 $3 }
    | Exp '*' Exp                               { TmTimes $1 $3 }
    | Exp idiv Exp                              { TmDiv $1 $3 }
    | Exp '>' Exp                               { TmCompare $3 $1 }
    | Exp '+' Exp                               { TmAdd $1 $3 }
    | Exp '==' Exp                              { TmEquals $1 $3 }
    | '-' Exp %prec NEG                         { TmNegative $2 }

    | fst Exp                                   { TmFst $2 }
    | snd Exp                                   { TmSnd $2 }
    | '(' Exp ',' Exp ')'                       { TmPair $2 $4 }

    | let '(' var ':' Type ')' '=' Exp ';' Exp  { TmLocal $3 $5 $8 $10 }
    | let '(' var ':' Type ')' '=' Exp          { TmLocal $3 $5 $8 TmContinue } 

    | var '=' Exp ';' Exp                       { TmLet $1 $3 $5}
    | var '=' Exp                               { TmLet $1 $3 TmContinue }
    
    
    | return Exp                                { TmReturn $2 }
    | continue                                  { TmContinue }


    | for '(' Exp to Exp ')' do '{' Exp '}' ';' Exp               { TmFor $3 $5 $9 $12 }
    | for '(' Exp to Exp ')' do '{' Exp '}'                       { TmFor $3 $5 $9 TmContinue }

    | while '(' Exp ')' do '{' Exp '}' ';' Exp                    { TmWhile $3 $7 $10 }
    | while '(' Exp ')' do '{' Exp '}'                            { TmWhile $3 $7 TmContinue }

    | if '(' Exp ')' then '{' Exp '}' else '{' Exp '}' ';' Exp    { TmFunkyIfElse $3 $7 $11 $14} 
    | if '(' Exp ')' then '{' Exp '}' else '{' Exp '}'            { TmIf $3 $7 $11 } 
    
    | if '(' Exp ')' then '{' Exp '}' ';' Exp                     { TmFunkyIf $3 $7 $10 } 
    | if '(' Exp ')' then '{' Exp '}'                             { TmIf $3 $7 TmContinue }


    | length Exp                                { TmLength $2 }
    | sum Exp                                   { TmSum $2 }
    | prod Exp                                  { TmProd $2 }
    | hstickall Exp                             { TmHStickAll $2 }
    | vstickall Exp                             { TmVStickAll $2 }
    | append Exp Exp                            { TmAppend $2 $3 }
    | remove Exp Exp                            { TmRemove $2 $3 }
    | modify Exp Exp Exp                        { TmModify $2 $3 $4 }
    | Exp '[' Exp ']'                           { TmIndex $1 $3 }   
    | int                                       { TmInt $1 } 
    | var                                       { TmVar $1 }
    | true                                      { TmTrue }
    | false                                     { TmFalse } 
    | '('')'                                    { TmUnit }
    | lam '(' var ':' Type ')' Exp              { TmLambda $3 $5 $7 } -- should we remove?
    | apply Exp Exp                             {  TmApp $2 $3 } 
    | '(' Exp ')'                               { $2 }
    | list '[' Exprs ']'                              { TmList $3 }

    
    | fun var '(' Params ')'  Type '{' Exp '}' ';' Exp          { TmFun $2 $4 $6 $8 $11 }
    | fun var '(' Params ')'  Type '{' Exp '}'                  { TmFun $2 $4 $6 $8 TmContinue }  
    
    
    | apply_fun var '(' Exprs ')'                                      { TmApply $2 (TmArgs $4) }
    | map var Exp                                    { TmMap $2 $3}
    --| create '[' array ']'                            { TmCreate $3 }



Params : Params_                                          { reverse $1 }
Params_  : Params_ ',' Param                                 { $3 : $1 }
       | Params_ ','                                     { $1 }
       | Param                                           { [ $1 ] }
       | {-empty-}                                     { [] }

Param : var ':' Type                                     { ($1, $3) }



Exprs : Exprs_                                          { reverse $1 }
Exprs_ : Exprs_ ',' Exp                                 { $3 : $1 }
       | Exprs_ ','                                     { $1 }
       | Exp                                           { [ $1 ] }
       | {-empty-}                                     { [] }


Type : Bool                     { NpBool } 
     | Int                      { NpInt } 
     | Unit                     { NpUnit }
     | '(' Type ',' Type ')'    { NpPair $2 $4 }
     | '[' Type ']'             { NpList $2 }
     | Type arr Type            { NpFun $1 $3 } 
     | NpTile                   { NpTile }
     | NpCell                   { NpCell }


{ 
parseError :: [NapToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data NapType = NpInt | NpBool | NpUnit | NpPair NapType NapType | NpList NapType | NpFun NapType NapType | NpTile | NpCell | NpNull
   deriving (Show,Eq)

type Environment = [[ (String,Expr) ]]

data Expr = TmInt Int | TmTrue | TmFalse | TmUnit | TmCompare Expr Expr | TmArray String
            | TmTile [String] | TmCell String
            | TmPair Expr Expr | TmAdd Expr Expr | TmVar String 
            | TmSub Expr Expr | TmTimes Expr Expr | TmDiv Expr Expr
            | TmFst Expr | TmSnd Expr
            | TmIf Expr Expr Expr | TmLet String Expr Expr
            | TmLambda String NapType Expr | TmApp Expr Expr 
            | Cl String NapType Expr Environment
            | TmRotate Expr Expr
            | TmHStick Expr Expr
            | TmVStick Expr Expr
            | TmImport String
            | TmSize Expr
            | TmHrepeat Expr Expr
            | TmVrepeat Expr Expr
            | TmHreflect Expr
            | TmVreflect Expr
            | TmBlank Expr
            | TmFull Expr
            | TmScale Expr Expr
            | TmNegate Expr
            | TmSubtile Expr Expr Expr Expr
            | TmInspect Expr Expr Expr
            | TmEquals Expr Expr
            | TmReplace Expr Expr Expr Expr
            | TmSquare Expr
            | TmFor Expr Expr Expr Expr
            | TmAnd Expr Expr
            | TmOr Expr Expr
            | TmStatic String NapType Expr Expr
            | TmAlter String NapType Expr
            | TmChange Expr Expr Expr Expr
            | TmPass
            | TmCreate Expr
            | TmAndBool Expr Expr
            | TmOrBool Expr Expr
            | TmNotBool Expr
            | TmTileEqual Expr Expr
            | TmList [Expr]
            | TmLength Expr
            | TmWhile Expr Expr Expr
            | TmLocal String NapType Expr Expr
            | TmFunkyIf Expr Expr Expr
            | TmReturn Expr
            | TmContinue
            | TmFunkyIfElse Expr Expr Expr Expr
            | TmIndex Expr Expr
            | TmSum Expr
            | TmProd Expr
            | TmHStickAll Expr
            | TmVStickAll Expr
            | TmAppend Expr Expr
            | TmRemove Expr Expr
            | TmModify Expr Expr Expr
            | TmFun String [(String,NapType)] NapType Expr Expr
            | TmApply String Expr
            | TmArgs [Expr]
            | TmMap String Expr
            | TmNegative Expr


    deriving (Show,Eq)
}

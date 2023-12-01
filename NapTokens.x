{ 
module NapTokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [\.a-zA-Z]    
-- alphabetic characters

tokens :-
$white+       ; 
  "--".*        ;  
  --The Tile Functions
  rotate         { tok (\p s -> TokenRotate p ) }
  hstick       { tok (\p s -> TokenHStick p ) }
  vstick       { tok (\p s -> TokenVStick p ) }
  size       { tok (\p s -> TokenSize p ) }
  hrepeat       { tok (\p s -> TokenHRepeat p ) }
  vrepeat       { tok (\p s -> TokenVRepeat p ) }
  hreflect       { tok (\p s -> TokenHReflect p ) }
  vreflect        { tok (\p s -> TokenVReflect p ) }
  blank         { tok (\p s -> TokenBlank p ) }
  full          { tok (\p s -> TokenFull p ) }
  scale         { tok (\p s -> TokenScale p ) }
  negate        { tok (\p s -> TokenNegate p ) }
  subtile        { tok (\p s -> TokenSubtile p ) }
  square      { tok (\p s -> TokenSquare p ) }
  Tequal       { tok (\p s -> TokenTileEqual p ) }
  Tand         { tok (\p s -> TokenAnd p ) }
  Tor         { tok (\p s -> TokenOr p ) }
  change        { tok (\p s -> TokenChange p ) }
  replace      { tok (\p s -> TokenReplace p ) }
  inspect       { tok (\p s -> TokenInspect p ) }


  create        { tok (\p s -> TokenCreate p) }

  import        { tok (\p s -> TokenImport p ) }

  --Boolean Ops
  and        { tok (\p s -> TokenAndBool p ) }
  or          { tok (\p s -> TokenOrBool p ) }
  not        { tok (\p s -> TokenNotBool p ) }

--Int ops
  "=="              { tok (\p s -> TokenEq p )}
  \>             { tok (\p s -> TokenGreaterThan p) }
  \<             { tok (\p s -> TokenLessThan p) }
  \-             { tok (\p s -> TokenMinus p) }
  \+             { tok (\p s -> TokenPlus p) }
  \*             { tok (\p s -> TokenTimes p) }
  \/           { tok (\p s -> TokenDiv p) }
--Pair ops
  fst            { tok (\p s -> TokenFst p) }
  snd            { tok (\p s -> TokenSnd p) }
  \,             { tok (\p s -> TokenComma p) }

  --sugar
  "||"           { tok (\p s -> TokenOrSugar p ) }
  "&&"           { tok (\p s -> TokenAndSugar p ) }
  "!"          { tok (\p s -> TokenNotSugar p ) }
  "T|"           { tok (\p s -> TokenTokOrSugar p )}
  "T&"           { tok (\p s -> TokenTokAndSugar p )}
  "T!"           { tok (\p s -> TokenTokNotSugar p )}

--Loops
  while        { tok (\p s -> TokenWhile p) }
  for         { tok (\p s -> TokenFor p ) }
  do         { tok (\p s -> TokenDo p ) }
  to         { tok (\p s -> TokenTo p ) }
  return        { tok (\p s -> TokenReturn p) }
  continue      { tok (\p s -> TokenContinue p) }

--Types
  Tile         { tok (\p s -> TokenTypeTile p) }
  Cell           { tok (\p s -> TokenTypeCell p) }
  Bool           { tok (\p s -> TokenTypeBool p)} 
  Int            { tok (\p s -> TokenTypeInt p) }
  Unit           { tok (\p s -> TokenTypeUnit p)}
  "->"           { tok (\p s -> TokenTypeArr p) }
  $digit+        { tok (\p s -> TokenInt p (read s)) }
  true           { tok (\p s -> TokenTrue p) }
  false          { tok (\p s -> TokenFalse p) }

--Control Flow
  if             { tok (\p s -> TokenIf p) }
  then           { tok (\p s -> TokenThen p) }
  else           { tok (\p s -> TokenElse p) }

  --Variable
  local       { tok (\p s -> TokenLocal p ) }
  let            { tok (\p s -> TokenLet p )}
  "="             { tok (\p s -> TokenAssign p) }
  \:             { tok (\p s -> TokenHasType p) }

--Other
  \(             { tok (\p s -> TokenLParen p) }
  \)             { tok (\p s -> TokenRParen p) }
  \[             { tok (\p s -> TokenLSquare p) }
  \]             { tok (\p s -> TokenRSquare p) }
  \{             { tok (\p s -> TokenLCurly p) }
  \}             { tok (\p s -> TokenRCurly p) }
  \;             { tok (\p s -> TokenSemiColon p) }


  apply       { tok (\p s -> TokenApply p ) }
  \\             { tok (\p s -> TokenLambda p) }
  list        { tok (\p s -> TokenList p ) }
  length      { tok (\p s -> TokenLength p ) }
  \!             { tok (\p s -> TokenExclam p) }
  sum         { tok (\p s -> TokenListSum p ) }
  prod       { tok (\p s -> TokenListProd p ) }
  hstickall  { tok (\p s -> TokenHStickAll p ) }
  vstickall  { tok (\p s -> TokenVStickAll p ) }
  append     { tok (\p s -> TokenAppend p ) }
  remove    { tok (\p s -> TokenRemove p ) }
  modify   { tok (\p s -> TokenModify p ) }
  fun        { tok (\p s -> TokenFun p ) }
  apply_fun       { tok (\p s -> TokenApplyFun p ) }
  map       { tok (\p s -> TokenMap p ) }


  $alpha [$alpha $digit \_ \â€™]*   { tok (\p s -> TokenVar p s) } 


{ 
-- Each action has type :: AlexPosn -> String -> MDLToken 

-- Helper function
tok:: (AlexPosn -> String -> NapToken) -> AlexPosn -> String -> NapToken
tok f p s = f p s

-- The token type: 
data NapToken =
--The Tile Functions
  TokenRotate AlexPosn           |
  TokenHStick AlexPosn          |
  TokenVStick AlexPosn        |
  TokenSize AlexPosn        |
  TokenHRepeat AlexPosn        |
  TokenVRepeat AlexPosn        |
  TokenHReflect AlexPosn        |
  TokenVReflect AlexPosn        |
  TokenBlank AlexPosn        |
  TokenFull AlexPosn        |
  TokenScale AlexPosn        |
  TokenNegate AlexPosn        | 
  TokenSubtile AlexPosn        |
  TokenSquare AlexPosn |
  TokenTileEqual AlexPosn |
  TokenAnd AlexPosn |
  TokenOr AlexPosn |
  TokenChange AlexPosn|
  TokenReplace AlexPosn|
  TokenInspect AlexPosn       |
  TokenCreate AlexPosn       |
  TokenImport AlexPosn          |

--Boolean Ops
  TokenAndBool AlexPosn |
  TokenOrBool AlexPosn |
  TokenNotBool AlexPosn |

--Int Ops
  TokenEq AlexPosn               |
  TokenMinus AlexPosn          |
  TokenGreaterThan AlexPosn    |
  TokenTimes AlexPosn          |
  TokenDiv AlexPosn            |
  TokenLessThan AlexPosn         |
  TokenPlus AlexPosn             |

--Pair Ops
  TokenFst AlexPosn              |
  TokenSnd AlexPosn              |
  TokenComma AlexPosn            | 

--Loops
  TokenFor AlexPosn |  
  TokenWhile AlexPosn |
  TokenDo AlexPosn |
  TokenTo AlexPosn |
  TokenReturn AlexPosn |
  TokenContinue AlexPosn |

--Types
  TokenTypeTile AlexPosn         |
  TokenTypeCell AlexPosn         |
  TokenTypeBool AlexPosn         | 
  TokenTypeInt  AlexPosn         | 
  TokenTypeUnit AlexPosn         |
  TokenTypeArr  AlexPosn         |
  TokenInt AlexPosn Int          | 
  TokenTrue AlexPosn             |
  TokenFalse AlexPosn            |

--Control Flow
  TokenIf AlexPosn               |
  TokenThen AlexPosn             |
  TokenElse AlexPosn             |

  --Variable
  TokenLocal AlexPosn |
  TokenLet AlexPosn              |
  TokenHasType AlexPosn          |

  --Other
  TokenLambda AlexPosn           |
  TokenVar AlexPosn String       |
  TokenArray AlexPosn String     |
  TokenApply AlexPosn |
  TokenList AlexPosn |
  TokenLength AlexPosn |
  TokenExclam AlexPosn |
  TokenListSum AlexPosn |
  TokenListProd AlexPosn |
  TokenHStickAll AlexPosn |
  TokenVStickAll AlexPosn |
  TokenAppend AlexPosn |
  TokenRemove AlexPosn |
  TokenModify AlexPosn |

  --sugar
  TokenOrSugar AlexPosn |
  TokenAndSugar AlexPosn |
  TokenTokOrSugar AlexPosn |
  TokenTokAndSugar AlexPosn |
  TokenNotSugar AlexPosn |
  TokenTokNotSugar AlexPosn |

  TokenLSquare AlexPosn |
  TokenRSquare AlexPosn |
  TokenLParen AlexPosn           |
  TokenRParen AlexPosn           |
  TokenLCurly AlexPosn |
  TokenRCurly AlexPosn |
  TokenAssign AlexPosn           |
  TokenSemiColon AlexPosn |
  TokenFun AlexPosn |
  TokenApplyFun AlexPosn |
  TokenMap AlexPosn 
  deriving (Eq,Show) 

tokenPosn :: NapToken -> String
--The Tile Functions
tokenPosn (TokenRotate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHStick (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVStick (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSize (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHRepeat (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVRepeat (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHReflect (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVReflect (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBlank (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFull (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenScale (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNegate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubtile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSquare (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTileEqual (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenChange (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReplace (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInspect (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenCreate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenImport (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


--Boolean Ops
tokenPosn (TokenAndBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOrBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


--Int Ops
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTimes (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiv (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


--Pair Ops
tokenPosn (TokenFst (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


--List Ops
tokenPosn (TokenFor (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDo (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTo (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReturn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenContinue (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


--Type Ops
tokenPosn (TokenTypeTile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeCell (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeInt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeUnit (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeArr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c) i) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


--Control Flow
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

--Variables
tokenPosn (TokenLet (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLocal (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHasType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

--Other
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLSquare (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRSquare (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLCurly (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRCurly (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemiColon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

tokenPosn (TokenLambda (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenArray (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenApply (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenList (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLength (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenExclam (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenListSum (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenListProd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHStickAll (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVStickAll (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAppend (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRemove (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenModify (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFun (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenApplyFun (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMap (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAssign (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOrSugar (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAndSugar (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTokAndSugar (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTokOrSugar (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotSugar (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTokNotSugar (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
}

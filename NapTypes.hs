module NapTypes where 
import NapGrammar
import Data.List (nubBy)


--data NapType = (NpInt,tenv) | NpBool | NpUnit | NpPair NapType NapType | NpFun NapType NapType | NpTile | NpCell
--   deriving (Show,Eq)

--type Environment = [ (String,fun_Expr) ]

--data Expr = TmInt Int | TmTrue | TmFalse | TmUnit | TmCompare Expr Expr 
--            | TmPair Expr Expr | TmAdd Expr Expr | TmVar String 
 --           | TmFst Expr | TmSnd Expr
--            | TmIf Expr Expr Expr | TmLet String NapType Expr Expr
 --           | TmLambda String NapType Expr | TmApp Expr Expr 
   --         | Cl String NapType Expr Environment
     --       | TmRotate Int Expr
       --     | TmHStick Expr Expr
--            | TmVStick Expr Expr
  --          | TmImport String
    --        | TmSize Expr
      --      | TmHrepeat Expr Expr
        --    | TmVrepeat Expr Expr
--            | TmHreflect Expr
  --          | TmVreflect Expr
    --        | TmBlank Expr
      --      | TmFull Expr
--            | TmScale Expr Expr
  ---         | TmNegate Expr
     --       | TmSubtile Expr Expr
       --     | TmInspect Expr Expr Expr
type TypeEnvironment = [ (String,NapType) ]
getBinding :: String -> TypeEnvironment -> NapType
getBinding x [] = error ("Variable binding not found"++ show x)
getBinding x ((y,t):tenv) | x == y  = t
                          | otherwise = getBinding x tenv

addBinding :: String -> NapType -> TypeEnvironment -> TypeEnvironment
addBinding x t tenv |checkBinding x t tenv= (x,t):tenv
                    | otherwise = error "Variable bound to other type"
checkBinding :: String -> NapType -> TypeEnvironment -> Bool
checkBinding x t [] = True
checkBinding x t ((y,t'):tenv) | x == y && t == t' = True
                               | x == y && t /= t' = False
                               | otherwise = checkBinding x t tenv

checkBindingPresent :: String -> TypeEnvironment -> Bool
checkBindingPresent x [] = False
checkBindingPresent x ((y,t):tenv) | x == y = True
                                   | otherwise = checkBindingPresent x tenv
isList :: NapType -> (Bool, NapType)
isList (NpList x) = (True,x)
isList _ = (False, NpUnit)
joinUnique :: [(String, NapType)] -> [(String, NapType)] -> [(String, NapType)]
joinUnique xs ys = nubBy (\(s1, _) (s2, _) -> s1 == s2) (xs ++ ys)

typeOf :: TypeEnvironment -> Expr -> (NapType,TypeEnvironment)
typeOf tenv (TmInt _ )  = (NpInt,tenv)

typeOf tenv (TmTrue ) = (NpBool,tenv)

typeOf tenv (TmFalse ) = (NpBool,tenv)

typeOf tenv (TmUnit ) = (NpUnit,tenv)

typeOf tenv (TmRotate i e) | (NpInt,NpTile) == (fst(typeOf tenv i), fst(typeOf tenv e)) = (NpTile,tenv)
                           | NpInt /= fst(typeOf tenv i) = error "Type Error in Rotate: First arg is not an int"
                           | NpTile /= fst(typeOf tenv e) = error "Type Error in Rotate: Second arg is not a tile"
                           | otherwise = error "Type Error in Rotate"

typeOf tenv (TmHStick e1 e2) | (NpTile,NpTile) == (fst (typeOf tenv e1), fst(typeOf tenv e2)) = (NpTile,tenv)
                             | NpTile /= fst(typeOf tenv e1) = error "Type Error in HStick: First arg is not a tile"
                             | NpTile /= fst(typeOf tenv e2) = error "Type Error in HStick: Second arg is not a tile"
                             | otherwise = error "Type Error in HStick"

typeOf tenv (TmVStick e1 e2) | (NpTile,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpTile,tenv)
                             | NpTile /= fst(typeOf tenv e1) = error "Type Error in HStick: First arg is not a tile"
                             | NpTile /= fst(typeOf tenv e2) = error "Type Error in HStick: Second arg is not a tile"
                             | otherwise = error "Type Error in VStick"

typeOf tenv (TmImport s) = (NpTile,tenv)

typeOf tenv (TmSub e1 e2) | (NpInt,NpInt) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpInt,tenv)
                          | NpInt /= fst(typeOf tenv e1) = error "Type Error in Sub: First arg is not an int"
                          | NpInt /= fst(typeOf tenv e2) = error "Type Error in Sub: Second arg is not an int"
                          | otherwise = error "Type Error in Subtract"

typeOf tenv (TmTimes e1 e2) | (NpInt,NpInt) == (fst (typeOf tenv e1), fst(typeOf tenv e2)) = (NpInt,tenv)
                          | NpInt /= fst(typeOf tenv e1) = error "Type Error in Times: First arg is not an int"
                          | NpInt /= fst(typeOf tenv e2) = error "Type Error in Times: Second arg is not an int"
                            | otherwise = error "Type Error in Times"                 

typeOf tenv (TmDiv e1 e2) | (NpInt,NpInt) == (fst(typeOf tenv e1), fst(typeOf tenv e2)) = (NpInt,tenv)
                          | NpInt /= fst(typeOf tenv e1) = error "Type Error in Sub: First arg is not an int"
                          | NpInt /= fst(typeOf tenv e2) = error "Type Error in Sub: Second arg is not an int"
                          | otherwise = error "Type Error in Divide"         

typeOf tenv (TmNegative e1) | NpInt == fst(typeOf tenv e1) = (NpInt,tenv)
                            | otherwise = error "Type Error in Negative: Arg is not an int"       

typeOf tenv (TmSize e) | (NpTile) == (fst(typeOf tenv e)) = (NpInt,tenv)
                       | otherwise = error "Type Error in Size: Arg is not a tile"              

typeOf tenv (TmHrepeat e1 e2) | (NpInt,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpTile,tenv)
                              | NpInt /= fst(typeOf tenv e1) = error "Type Error in Hrepeat: First arg is not an int"
                              | NpTile /= fst(typeOf tenv e2) = error "Type Error in Hrepeat: Second arg is not a tile"
                              | otherwise = error "Type Error in Hrepeat"           

typeOf tenv (TmVrepeat e1 e2) | (NpInt,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpTile,tenv)
                              | NpInt /= fst(typeOf tenv e1) = error "Type Error in Vrepeat: First arg is not an int"
                              | NpTile /= fst(typeOf tenv e2) = error "Type Error in Vrepeat: Second arg is not a tile"
                              | otherwise = error "Type Error in Vrepeat"         

typeOf tenv (TmHreflect e) | (NpTile) == (fst(typeOf tenv e)) = (NpTile,tenv)
                            | otherwise = error "Type Error in Hreflect: Arg is not a tile"              

typeOf tenv (TmVreflect e) | (NpTile) == (fst(typeOf tenv e)) = (NpTile,tenv)
                            | otherwise = error "Type Error in Vreflect: Arg is not a tile"          

typeOf tenv (TmBlank e) | (NpInt) == (fst(typeOf tenv e)) = (NpTile,tenv)
                          | otherwise = error "Type Error in Blank: Arg is not an int"            

typeOf tenv (TmFull e) | (NpInt) == (fst(typeOf tenv e)) = (NpTile,tenv)
                          | otherwise = error "Type Error in Full: Arg is not an int"          

typeOf tenv (TmScale e1 e2) | (NpInt,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpTile,tenv)
                              | NpInt /= fst(typeOf tenv e1) = error "Type Error in Scale: First arg is not an int"
                              | NpTile /= fst(typeOf tenv e2) = error "Type Error in Scale: Second arg is not a tile"
                            | otherwise = error "Type Error in Scale"

typeOf tenv (TmNegate e) | (NpTile) == (fst(typeOf tenv e)) = (NpTile,tenv)
                          | otherwise = error "Type Error in Negate: Arg is not a tile"              

typeOf tenv (TmReplace e1 e2 e3 e4) | (NpInt,NpInt,NpTile,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2),(fst( typeOf tenv e3)),fst(typeOf tenv e4)) = (NpTile,tenv)
                                    | NpInt /= fst(typeOf tenv e1) = error "Type Error in Replace: First arg is not an int"
                                    | NpInt /= fst(typeOf tenv e2) = error "Type Error in Replace: Second arg is not an int"
                                    | NpTile /= fst(typeOf tenv e3) = error "Type Error in Replace: Third arg is not a tile"
                                    | NpTile /= fst(typeOf tenv e4) = error "Type Error in Replace: Fourth arg is not a tile"
                                    | otherwise = error "Type Error in Replace"

typeOf tenv (TmSubtile e1 e2 e3 e4) | (NpInt,NpInt, NpInt,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2),(fst( typeOf tenv e3)),(fst( typeOf tenv e4))) = (NpTile,tenv)
                                    | NpInt /= fst(typeOf tenv e1) = error "Type Error in Subtile: First arg is not an int"
                                    | NpInt /= fst(typeOf tenv e2) = error "Type Error in Subtile: Second arg is not an int"
                                    | NpInt /= fst(typeOf tenv e3) = error "Type Error in Subtile: Third arg is not an int"
                                    | NpTile /= fst(typeOf tenv e4) = error "Type Error in Subtile: Fourth arg is not a tile"
                                  | otherwise = error "Type Error in Subtile"             

typeOf tenv (TmInspect e1 e2 e3) | (NpInt, NpInt,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2),(fst( typeOf tenv e3))) = (NpInt,tenv)
                                  | NpInt /= fst(typeOf tenv e1) = error "Type Error in Inspect: First arg is not an int"
                                  | NpInt /= fst(typeOf tenv e2) = error "Type Error in Inspect: Second arg is not an int"
                                  | NpTile /= fst(typeOf tenv e3) = error "Type Error in Inspect: Third arg is not a tile"
                                  | otherwise = error "Type Error in Inspect"         

typeOf tenv (TmChange e1 e2 e3 e4) | (NpInt,NpInt,NpInt,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2),(fst( typeOf tenv e3)),fst (typeOf tenv e4)) = (NpTile,tenv)
                                    | NpInt /= fst(typeOf tenv e1) = error "Type Error in Change: First arg is not an int"
                                    | NpInt /= fst(typeOf tenv e2) = error "Type Error in Change: Second arg is not an int"
                                    | NpInt /= fst(typeOf tenv e3) = error "Type Error in Change: Third arg is not an int"
                                    | NpTile /= fst(typeOf tenv e4) = error "Type Error in Change: Fourth arg is not a tile"
                                    | otherwise = error "Type Error in Change"          

typeOf tenv (TmEquals e1 e2) | (NpInt,NpInt) == (fst (typeOf tenv e1), fst (typeOf tenv e2))  = (NpBool,tenv)
                              | NpInt /= fst(typeOf tenv e1) = error "Type Error in Equals: First arg is not an int"
                              | NpInt /= fst(typeOf tenv e2) = error "Type Error in Equals: Second arg is not an int"
                              | otherwise = error "Type Error in Equals"          

typeOf tenv (TmCompare e1 e2) | (NpInt,NpInt) == (fst (typeOf tenv e1), fst (typeOf tenv e2))  = (NpBool,tenv)
                              | NpInt /= fst(typeOf tenv e1) = error "Type Error in Compare: First arg is not an int"
                              | NpInt /= fst(typeOf tenv e2) = error "Type Error in Compare: Second arg is not an int"
                                | otherwise = error "Type Error in Compare"         

typeOf tenv (TmSquare e) | (NpTile ==fst( typeOf tenv e)) = (NpTile,tenv)
                          | otherwise = error "Type Error in Square: Arg is not a tile"            

typeOf tenv (TmPair e1 e2) = (NpPair t1 t2,tenv )
  where (t1,t2) = (fst (typeOf tenv e1), fst (typeOf tenv e2)) 

typeOf tenv (TmFst e1) = (t1,tenv )
  where (NpPair t1 t2) = fst (typeOf tenv e1)

typeOf tenv (TmSnd e2) = (t2,tenv)
  where (NpPair t1 t2) = fst (typeOf tenv e2)

typeOf tenv (TmAdd e1 e2) | (NpInt,NpInt) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpInt,tenv)
                          | NpInt /= fst(typeOf tenv e1) = error "Type Error in Add: First arg is not an int"
                          | NpInt /= fst(typeOf tenv e2) = error "Type Error in Add: Second arg is not an int"
                          | otherwise = error "Type Error in Add"

typeOf tenv (TmVar x) = (getBinding x tenv,tenv)



typeOf tenv (TmLambda x t e) = (NpFun t u ,tenv)
  where u = fst (typeOf (addBinding x t tenv) e)

typeOf tenv (TmApp e1 e2) | t1 == t3 =( t2,tenv)
                          | otherwise = error "Type Error in App"
  where ((t1,t2),t3) = (checkFun (fst (typeOf tenv e1)), fst (typeOf tenv e2))
        checkFun (NpFun t1 t2) = (t1,t2)
        checkFun _ = error "Type Error"

typeOf tenv (TmLet x e1 e2) | getBinding x tenv == t1 = typeOf tenv e2
                            | otherwise = error "Type Error in Let: Type Mismatch"
  where t1 = fst (typeOf tenv e1)


typeOf tenv (TmLocal x t e1 e2) | t == t1 = typeOf (addBinding x t tenv) e2
                                | otherwise = error "Type Error in Let"
  where t1 = fst (typeOf tenv e1)
--TODO figure out how to type check this part


typeOf tenv (TmAnd e1 e2) | (NpTile,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpTile,tenv)
                          | NpTile /= fst(typeOf tenv e1) = error "Type Error in Tile And: First arg is not a tile"
                          | NpTile /= fst(typeOf tenv e2) = error "Type Error in Tile And: Second arg is not a tile"
                          | otherwise = error "Type Error in Tile And"

typeOf tenv (TmOr e1 e2) | (NpTile,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpTile,tenv)
                         | NpTile /= fst(typeOf tenv e1) = error "Type Error in Tile Or: First arg is not a tile"
                          | NpTile /= fst(typeOf tenv e2) = error "Type Error in Tile Or: Second arg is not a tile"
                          | otherwise = error "Type Error in Tile Or"               


typeOf tenv (TmAndBool e1 e2) | (NpBool,NpBool) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpBool,tenv)
                              | NpBool /= fst(typeOf tenv e1) = error "Type Error in Bool And: First arg is not a bool"
                              | NpBool /= fst(typeOf tenv e2) = error "Type Error in Bool And: Second arg is not a bool"
                              | otherwise = error "Type Error in Bool And"

typeOf tenv (TmOrBool e1 e2) | (NpBool,NpBool) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpBool,tenv)
                              | NpBool /= fst(typeOf tenv e1) = error "Type Error in Bool Or: First arg is not a bool"
                              | NpBool /= fst(typeOf tenv e2) = error "Type Error in Bool Or: Second arg is not a bool"
                              | otherwise = error "Type Error in Bool Or"

typeOf tenv (TmNotBool e) | (NpBool) == (fst(typeOf tenv e)) = (NpBool,tenv)
                          | otherwise = error "Type Error in Bool Not"


typeOf tenv (TmTileEqual e1 e2 ) | (NpTile,NpTile) == (fst (typeOf tenv e1), fst (typeOf tenv e2)) = (NpBool,tenv)
                                 | NpTile /= fst(typeOf tenv e1) = error "Type Error in Tile Equal: First arg is not a tile"
                                  | NpTile /= fst(typeOf tenv e2) = error "Type Error in Tile Equal: Second arg is not a tile"
                                  | otherwise = error "Type Error in Tile Equal"

typeOf tenv (TmList exprs) | checkList exprs = (NpList t1,tenv)
                           | otherwise = error "Type Error in List"                  
  where (t1,tenv') = if not (null exprs) then typeOf tenv (head exprs) else (NpNull,tenv)
        checkList [] = True
        checkList (x:xs) = (t1 == fst(typeOf tenv x)) && checkList xs

typeOf tenv (TmLength e) | fst (isList (fst (typeOf tenv e))) = (NpInt,tenv)
                         | not (fst (isList (fst (typeOf tenv e)))) = error "Type Error in Length: argument is not a list"
                         | otherwise = error "Type Error in Length"



typeOf tenv (TmSum e ) | fst (isList (fst (typeOf tenv e))) && (snd (isList (fst (typeOf tenv e))))==NpInt = (NpInt,tenv)
                         | not (fst (isList (fst (typeOf tenv e)))) = error "Type Error in Sum: argument is not a list"
                         | (snd (isList (fst (typeOf tenv e)))) /= NpInt = error "Type Error in Sum: argument is not a list of ints"
                       | otherwise = error "Type Error in Sum"

typeOf tenv (TmProd e ) | fst (isList (fst (typeOf tenv e))) && (snd (isList (fst (typeOf tenv e))))==NpInt = (NpInt,tenv)
                         | not (fst (isList (fst (typeOf tenv e)))) = error "Type Error in Prod: argument is not a list"
                         | (snd (isList (fst (typeOf tenv e)))) /= NpInt = error "Type Error in Prod: argument is not a list of ints"
                        | otherwise = error "Type Error in Prod"
      
typeOf tenv (TmHStickAll e ) | fst (isList (fst (typeOf tenv e))) && (snd (isList (fst (typeOf tenv e))))==NpTile = (NpTile,tenv)
                         | not (fst (isList (fst (typeOf tenv e)))) = error "Type Error in HStickAll: argument is not a list"
                         | (snd (isList (fst (typeOf tenv e)))) /= NpTile = error "Type Error in HStickAll: argument is not a list of tiles"
                             | otherwise = error "Type Error in HStickAll"

typeOf tenv (TmVStickAll e ) | fst (isList (fst (typeOf tenv e))) && (snd (isList (fst (typeOf tenv e))))==NpTile = (NpTile,tenv)
                         | not (fst (isList (fst (typeOf tenv e)))) = error "Type Error in VStickAll: argument is not a list"
                         | (snd (isList (fst (typeOf tenv e)))) /= NpTile = error "Type Error in VStickAll: argument is not a list of tiles"
                            
                             | otherwise = error "Type Error in VStickAll"

typeOf tenv (TmCreate e) | fst (isList (fst (typeOf tenv e))) && (snd (isList (fst (typeOf tenv e))))==NpInt = (NpTile,tenv)
                         | not (fst (isList (fst (typeOf tenv e)))) = error "Type Error in Create: argument is not a list"
                         | (snd (isList (fst (typeOf tenv e)))) /= NpInt = error "Type Error in Create: argument is not a list of ints"
                         | otherwise = error "Type Error in Create"

typeOf tenv (TmAppend e1 e2) | fst (isList (fst (typeOf tenv e1))) && (snd (isList (fst (typeOf tenv e1)))) == (fst (typeOf tenv e2)) = (NpList (fst (typeOf tenv e2)),tenv)
                              |not (fst (isList (fst (typeOf tenv e1)))) = error "Type Error in Append: first argument is not a list"      
                              | (snd (isList (fst (typeOf tenv e1)))) /= (fst (typeOf tenv e2)) = error "Type Error in Append: second argument is not the same type as the list"
                             | otherwise = error "Type Error in Append"

typeOf tenv (TmRemove e1 e2) | fst (isList (fst (typeOf tenv e1))) && NpInt == (fst (typeOf tenv e2)) = (NpList( snd (isList (fst (typeOf tenv e1)))),tenv)
                              |not  (fst (isList (fst (typeOf tenv e1)))) = error "Type Error in Remove: first argument is not a list"
                              | NpInt /= (fst (typeOf tenv e2)) = error "Type Error in Remove: second argument is not an integer"
                             | otherwise = error "Type Error in Remove"

typeOf tenv (TmModify e1 e2 e3) | fst (isList (fst (typeOf tenv e1))) && NpInt == (fst (typeOf tenv e2)) && (snd (isList (fst (typeOf tenv e1)))) == (fst (typeOf tenv e3)) = (NpList (fst (typeOf tenv e3)),tenv)
                                | not (fst (isList (fst (typeOf tenv e1)))) = error "Type Error in Modify: first argument is not a list"
                                | NpInt /= (fst (typeOf tenv e2)) = error "Type Error in Modify: second argument is not an integer"
                                | (snd (isList (fst (typeOf tenv e1)))) /= (fst (typeOf tenv e3)) = error "Type Error in Modify: third argument is not same type as the list"
                                | otherwise = error "Type Error in Modify"

typeOf tenv (TmIndex list pos ) | fst (isList (fst (typeOf tenv list))) && (NpInt) == (fst (typeOf tenv pos)) = (t1,tenv)
                                | not (fst (isList (fst (typeOf tenv list)))) = error "Type Error in Index: first argument is not a list"
                                | (NpInt) /= (fst (typeOf tenv pos)) = error "Type Error in Index: second argument is not an integer"
                                | otherwise = error "Type Error in Index"
  where (NpList t1) = fst (typeOf tenv list)

typeOf tenv (TmFunkyIf e1 e2 e3) | (NpBool) == (fst (typeOf tenv e1)) = typeOf tenv' e3
                                 | NpBool /= (fst (typeOf tenv e1)) = error "Type Error in If: condition is not a boolean"
  where tenv' = snd(typeOf tenv e2)


typeOf tenv (TmWhile e1 e2 e3) | (NpBool) == (fst (typeOf tenv e1)) = typeOf tenv' e3
                               | NpBool /= (fst (typeOf tenv e1)) = error "Type Error in While: condition is not a boolean"
  where tenv' = snd(typeOf tenv e2)
typeOf tenv (TmFor start finish done next) | (NpInt,NpInt) == (fst(typeOf tenv start), fst(typeOf tenv finish))= typeOf tenv' next 
                                           | NpInt /= (fst(typeOf tenv start)) = error "Type Error in For: start is not an integer"
                                           | NpInt /= (fst(typeOf tenv finish)) = error "Type Error in For: finish is not an integer"
  where tenv' = snd(typeOf tenv done)



typeOf tenv (TmFun func_name args func_type func_expr nextE) | func_type == expr_type = typeOf tenv' nextE
  where expr_type = fst(typeOf (args ++ tenv') func_expr)
        tenv' = tenv ++ createFuncParamTypes func_name args 0 ++ [(func_name, func_type)]
       

typeOf tenv (TmApply funcName (TmArgs args)) | checkFunctionParamTypes tenv funcName args 0 = (getBinding funcName tenv,tenv)


typeOf tenv (TmMap funcName list) |getBinding (assembleString funcName 0) tenv == listHead && checkFunc funcName 1 tenv= (NpList (getBinding funcName tenv),tenv)
                                  | otherwise = error "Type Error in Map: argument is not a valid list"
  where headExpr :: NapType -> NapType
        headExpr (NpList t) = t
        listHead = headExpr (fst(typeOf tenv list))


typeOf tenv TmContinue = (NpUnit,tenv)

typeOf tenv (TmFunkyIfElse bool fex seex finex) | (NpBool) == (fst (typeOf tenv bool)) = typeOf tenv' finex
                                                | NpBool /= (fst (typeOf tenv bool)) = error "Type Error in IfElse: condition is not a boolean"
  where tenv' = joinUnique (snd(typeOf tenv fex)) (snd(typeOf tenv seex))

typeOf tenv (TmIf e1 e2 e3) | NpBool == fst(typeOf tenv e1) &&t2 == t3 = (t2,tenv)
                            | NpBool /= fst(typeOf tenv e1) = error "Type Error in If: condition is not a boolean"
                            | t2 /= t3 = error "Type Error in If: then and else expressions are not the same type"
                            | otherwise = error "Type Error in If"
  where (t2,t3) = (fst (typeOf tenv e2),(fst( typeOf tenv e3)))



typeOf tenv (TmReturn e) = (fst (typeOf tenv e),tenv)
 
typeOf tenv _ = error "Type Error"
--check that a funciton only has one intput paramaeter 
checkFunc :: String ->Int->TypeEnvironment->  Bool
checkFunc funcName 0 tenv= checkBindingPresent (assembleString funcName 0) tenv
checkFunc funcName n tenv | not (checkBindingPresent (assembleString funcName n) tenv) = checkFunc funcName (n-1) tenv
                          | otherwise = False


listType :: NapType -> NapType
listType (NpList t) = t


checkFunctionParamTypes::[(String,NapType)]-> String -> [Expr] -> Int-> Bool 
checkFunctionParamTypes tenv funcName [] n= True
checkFunctionParamTypes tenv funcName (x:xs) n | getBinding (assembleString funcName n) tenv == fst(typeOf tenv x) = checkFunctionParamTypes tenv funcName xs (n+1)
                                               | otherwise = False 


assembleString :: String  -> Int ->  String 
assembleString str n = str ++ "^" ++ (show n)


createFuncParamTypes :: String -> [(String,NapType)] ->Int -> [(String,NapType)]
createFuncParamTypes func_name [] n = []
createFuncParamTypes func_name ((x,t):xs) n = (assembleString func_name n,t) : createFuncParamTypes func_name xs (n+1)

-- Function for printing the results of the TypeCheck
unparseType :: NapType -> String
unparseType NpBool = "Bool"
unparseType NpInt = "Int"
unparseType NpUnit = "Unit"
unparseType (NpPair t1 t2) = "( " ++ (unparseType t1) ++ " , " ++ (unparseType t2) ++ " )"
unparseType (NpList ts) = "[" ++ (unparseType ts) ++ "]"
unparseType (NpFun t1 t2) = (unparseType t1) ++ " -> " ++ (unparseType t2)
unparseType NpTile = "Tile"
unparseType NpCell = "Cell"
unparseType NpNull = "Null"

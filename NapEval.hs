module NapEval where
import NapGrammar
import Data.List


--Data structures as defined in NapGrammar:
--data NapType = TyInt | TyBool | TyUnit | TyPair NapType NapType | TyFun NapType NapType
--type Environment = [[ (String,Expr) ]]
--data Expr = TmInt Int | TmTrue | TmFalse | TmUnit | TmCompare Expr Expr 
--           | TmPair Expr Expr | TmAdd Expr Expr | TmVar String 
--           | TmFst Expr | TmSnd Expr
--           | TmIf Expr Expr Expr | TmLet String NapType Expr Expr
--           | TmLambda String NapType Expr | TmApp Expr Expr
--           | Cl ( String NapType Expr Environment)

data Frame = HCompare Expr | CompareH Expr
           | HEquals Expr  | EqualsH Expr 
           | HAdd Expr  | AddH Expr
           | HSub Expr  | SubH Expr
           | HTimes Expr  | TimesH Expr
           | HDiv Expr  | DivH Expr
           | HPair Expr  | PairH Expr
           | HList Expr  | ListH Expr
           | FstH | SndH 
           | HIf Expr Expr  | HLet String Expr 
           | HLocal String NapType Expr 
           | HStatic String NapType Expr 
           | HApp Expr  | AppH Expr
           | HRotate Expr  | RotateH Expr
           | HHStick Expr  | HStickH Expr
           | HVStick Expr  | VStickH Expr
           | HSize 
           | HHRepeat Expr  | HRepeatH Expr
           | HVRepeat Expr  | VRepeatH Expr
           | HHReflect
           | HVReflect 
           | HBlank 
           | HFull
           | HScale Expr  | ScaleH Expr
           | HNegate 
           | HSubtile Expr Expr Expr  | SubHtile Expr Expr Expr  | SubtileH Expr Expr Expr  | SubtileHH Expr Expr Expr
           | HReplace Expr Expr Expr  | ReplHace Expr Expr Expr  | ReHplace Expr Expr Expr  | ReplaceH Expr Expr Expr
           | HInspect Expr Expr  | InspHect Expr Expr  | InHspect Expr Expr 
           | HImport Expr  | ImportH Expr
           | HSquare 
           | HFor  Expr Expr Expr  
           | FHor  Expr Expr Expr  
           | FoHr  Expr Expr Expr 
           | ForH  Expr Expr Expr 
           | ForHH Expr Expr Expr
           | HAnd Expr  | AndH Expr
           | HOr Expr  | OrH Expr
           | HAlter String NapType 
           | HChange Expr Expr Expr  | ChHange Expr Expr Expr 
           | ChanHge Expr Expr Expr  | ChangeH Expr Expr Expr
           | HandBool Expr  | AndBoolH Expr 
           | HorBool Expr  | OrBoolH Expr
           | HNot
           | HTEquals Expr | EqualsTH Expr
           | HLength
           | HFunkyIf Expr Expr
           | HFunkyIfElse Expr Expr Expr
           | HIndex Expr | InHdex Expr
           | HSum | HProd | HMap String | HHStickAll | HVStickAll | HCreate| HAppend Expr |HNegative
           | HRemove Expr | RemoveH Expr
           | HModify Expr Expr | ModifyH Expr Expr
           |PASS


-- TODO: Finish of inspect (takes a coord and a tile and returns the value at that coord)
--       Static variables, for loops 


type Kontinuation = [ Frame ]
type State = (Expr,Environment,Kontinuation)

type FunExpr = (Expr)

-- Function to unpack a closure to extract the underlying lambda term and environment
unpack :: Expr -> (Expr,Environment)
unpack (Cl x t e env1) = ((TmLambda x t e) , env1)
unpack e = (e,[])

-- Look up a value in an environment and unpack it
getValueBinding :: String -> Environment -> Environment -> (Expr,Environment)
getValueBinding x [] env = error ("Variable binding not found"++show x++(show env))
getValueBinding x (y:ys) env| fst (getValue x y) = snd (getValue x y)
                            | otherwise = getValueBinding x ys env

-- Look up a value in an environment
getValue :: String -> [ (String,Expr) ] -> (Bool,(Expr,Environment))
getValue x [] = (False , (TmUnit,[]))
getValue x ((y,e):env) | x == y  = (True, unpack e)
                       | otherwise = getValue x env

-- Check if value in all environments
checkValueBinding :: String -> Environment -> Bool
checkValueBinding x [] = False
checkValueBinding x (y:ys) | fst (getValue x y) = True
                           | otherwise = checkValueBinding x ys


--check if value in a single environment
checkValue :: String -> [ (String,Expr) ] -> Bool
checkValue x [] = False
checkValue x ((y,e):env) | x == y = True
                         | otherwise = checkValue x env

--update value in all environments. If not in any, add it to the latest environment
update :: Environment -> String -> Expr -> Environment
update env x e | checkValueBinding x env = updateCorrectEnv x e env
               | otherwise = error "Variable not defined in environment"
               where 
                  updateCorrectEnv :: String -> Expr -> Environment -> Environment
                  updateCorrectEnv x e [] = []
                  updateCorrectEnv x e (y:ys) | checkValue x y = (update' x e y) : ys
                                              | otherwise = y : (updateCorrectEnv x e ys)

                  
                  
update' :: String -> Expr -> [ (String,Expr) ] -> [ (String,Expr) ]
update' x e [] = []
update' x e ((y,e'):env) | x == y = (y,e) : env
                         | otherwise = (y,e') : (update' x e env)
 

localUpdate :: Environment -> String -> Expr -> Environment
localUpdate env x e | checkValue x (head env) =error "Variable already defined in local environment"
                    | otherwise = ((x,e) : (head env)) : (tail env)
                  
                        
                        


-- Checks for terminated expressions
isValue :: Expr -> Bool
isValue (TmInt _) = True
isValue TmTrue = True
isValue TmFalse = True
isValue TmUnit = True
isValue (TmPair e1 e2) = isValue e1 && isValue e2
isValue (TmList _ ) = True 
isValue (Cl _ _ _ _) = True
isValue( TmTile _ )= True
isValue (TmCell _) = True
isValue _ = False


--Small step evaluation function
eval1 :: State -> IO State
eval1 ((TmVar x),env,k) = do  
                            return (e',env'++env,k) 
                    where (e',env') = getValueBinding x env env--print ("value"++show e')
                                    --print ("senv"++show senv)

eval1 ((TmImport fileName),env,k) = do x <- importTile fileName
                                       if checkTile x && checkValues x then return (TmTile x,env,k)
                                       else error "Invalid tile"
                      

eval1 ((TmRotate e1 e2),env,k) = return (e1,[]:env,(HRotate e2):k)
eval1 ((TmInt e1),env,(HRotate e2):k) = return (e2,[]:(tail (env)),(RotateH (TmInt e1)) : k)
eval1 ((TmTile e2),env,(RotateH (TmInt e1)):k)= return (TmTile (rotateTile e1 e2 ),tail env,k)

eval1 ((TmHStick e1 e2),env,k) = return (e1,[]:env,(HHStick e2):k)
eval1 ((TmTile e1),env,(HHStick e2 ):k) = return (e2,[]:(tail (env)),(HStickH (TmTile e1)) : k)
eval1 ((TmTile e2),env,(HStickH (TmTile e1)):k)= return (TmTile (hstickTile e1 e2 ),tail env,k)


eval1 ((TmVStick e1 e2),env,k) = return (e1,[]:env,(HVStick e2):k)
eval1 ((TmTile e1),env,(HVStick e2 ):k) = return (e2,[]:(tail (env)),(VStickH (TmTile e1)) : k)
eval1 ((TmTile e2),env,(VStickH (TmTile e1)):k)= return (TmTile (vstickTile e1 e2 ),tail env,k)

eval1 ((TmSize e1),env,k) = return (e1,[]:env,(HSize):k)
eval1 ((TmTile e1),env,(HSize ):k)= return (TmInt (length e1),tail env,k)

eval1 ((TmHrepeat e1 e2),env,k) = return (e1,[]:env,(HHRepeat e2):k)
eval1 ((TmInt num), env,(HHRepeat e2 ):k) = return (e2,[]:(tail (env)),(HRepeatH (TmInt num)) : k)
eval1 ((TmTile e2),env,(HRepeatH (TmInt num)):k)= return (TmTile (hrepeatTile num e2 ),tail env,k)

eval1 ((TmVrepeat e1 e2),env,k) = return (e1,[]:env,(HVRepeat e2):k)
eval1 ((TmInt num), env,(HVRepeat e2 ):k) = return (e2,[]:(tail (env)),(VRepeatH (TmInt num)) : k)
eval1 ((TmTile e1), env,(VRepeatH (TmInt num)):k)= return (TmTile (vrepeatTile num e1 ),tail env,k)

eval1 ((TmHreflect e1),env,k) = return (e1,[]:env,(HHReflect):k)
eval1 ((TmTile e1),env,(HHReflect):k)= return (TmTile (hreflectTile e1 ),tail env,k)

eval1 ((TmVreflect e1),env,k) = return (e1,[]:env,(HVReflect):k)
eval1 ((TmTile e1 ),env,(HVReflect):k)= return (TmTile (vreflectTile e1 ),tail env,k)

eval1 ((TmBlank e1),env,k) = return (e1,[]:env,(HBlank):k)
eval1 ((TmInt e1),env,(HBlank):k)= return (TmTile (blankTile e1 ),tail env,k)

eval1 ((TmFull e1),env,k) = return (e1,[]:env,(HFull):k)
eval1 ((TmInt e1),env,(HFull):k)= return (TmTile (fullTile e1 ),tail env,k)

eval1 ((TmScale e1 e2),env,k) = return (e1,[]:env,(HScale e2):k)
eval1 ((TmInt e1),env,(HScale e2 ):k) = return (e2,[]:(tail (env)),(ScaleH (TmInt e1)) : k)
eval1 ((TmTile e2),env,(ScaleH (TmInt e1)):k)= return (TmTile (scaleTile e1 e2 ),tail env,k)

eval1 ((TmNegate e1), env, k) = return (e1, []:env,(HNegate):k)
eval1 ((TmTile e1), env,(HNegate):k) = return (TmTile (negateTile e1),tail env, k)

eval1 ((TmSubtile e1 e2 e3 e4), env, k) = return (e1, []:env, (HSubtile e2 e3 e4):k)
eval1 ((TmInt e1), env, (HSubtile e2 e3 e4):k) = return (e2, []:(tail (env)), (SubHtile (TmInt e1) e3 e4):k)
eval1 ((TmInt e2), env, (SubHtile (TmInt e1) e3 e4):k) = return (e3, []:(tail (env)), (SubtileH (TmInt e1) (TmInt e2) e4):k)
eval1 ((TmInt e3), env, (SubtileH (TmInt e1) (TmInt e2) e4):k) = return (e4, []:(tail (env)), (SubtileH (TmInt e1) (TmInt e2) (TmInt e3)):k)
eval1 ((TmTile e3), env, (SubtileH (TmInt p1) (TmInt p2) (TmInt e1)):k) = return (TmTile (subtileTile e1 p1 p2 e3),tail env, k)


eval1 ((TmSquare e1), env,k) = return (e1, []:env, (HSquare):k)
eval1 ((TmTile e1), env, (HSquare):k) = return (TmTile (squareTile e1),tail env, k)



eval1 ((TmAndBool e1 e2),env,k) = return (e1,[]:env,(HandBool e2):k)
eval1 ((TmTrue),env1,(HandBool e2 ):k) = return (e2,[]:(tail (env1)),(AndBoolH (TmTrue)) : k)
eval1 ((TmFalse),env,(HandBool e2 ):k) = return (TmFalse,tail env,k)
eval1 ((TmFalse),env,(AndBoolH (TmTrue)):k) = return (TmFalse,tail env,k)
eval1 ((TmTrue),env,(AndBoolH (TmTrue)):k) = return (TmTrue,tail env,k)


eval1 ((TmOrBool e1 e2),env,k) = return (e1,[]:env,(HorBool e2):k)
eval1 ((TmTrue),env1,(HorBool e2 ):k) = return (TmTrue,tail env1,k)
eval1 ((TmFalse),env,(HorBool e2 ):k) = return (e2,[]:(tail (env)),(OrBoolH (TmFalse)) : k)
eval1 ((TmTrue),env,(OrBoolH (TmFalse)):k) = return (TmTrue,tail env,k)
eval1 ((TmFalse),env,(OrBoolH (TmFalse)):k) = return (TmFalse,tail env,k)

eval1 ((TmNotBool e1),env,k) = return (e1,[]:env,(HNot):k)
eval1 ((TmTrue ),env,(HNot):k) = return (TmFalse,tail env,k)
eval1 ((TmFalse ),env,(HNot):k) = return (TmTrue,tail env,k)

eval1 ((TmTileEqual e1 e2 ),env,k) = return (e1,[]:env,(HTEquals e2):k)
eval1 ((TmTile x ),env1,(HTEquals e2 ):k) = return (e2,[]:(tail (env1)),(EqualsTH (TmTile x)) : k)
eval1 ((TmTile y ),env,(EqualsTH (TmTile x)):k) | x == y = return (TmTrue,tail env,k)
                                                     | otherwise = return (TmFalse,tail env,k)




                  
-- Rule for terminated evaluations
eval1 (v,env,[]) | isValue v = return (v,env,[])

eval1 ((TmEquals e1 e2),env, k) = return (e1,[]:env,(HEquals e2):k)
eval1 ((TmInt n),env1,(HEquals e ):k) = return (e,[]:(tail (env1)),(EqualsH (TmInt n)) : k)
eval1 ((TmInt m),env,(EqualsH (TmInt n)):k) | n == m = return (TmTrue,tail env,k)
                                             | otherwise = return (TmFalse,tail env,k)

-- Evaluation rules for less than operator
eval1 ((TmCompare e1 e2),env,k) = return (e1,[]:env,(HCompare e2):k)
eval1 ((TmInt n),env1,(HCompare e ):k) = return (e,[]:(tail (env1)),(CompareH (TmInt n)) : k)
eval1 ((TmInt m),env,(CompareH (TmInt n)):k) | n < m = return (TmTrue,tail env,k)
                                             | otherwise = return (TmFalse,tail env,k)

-- Evaluation rules for plus operator
eval1 ((TmAdd e1 e2),env,k) = return (e1,[]:env,(HAdd e2):k)
eval1 ((TmInt n),env1,(HAdd e ):k) = return (e,[]:(tail (env1)),(AddH (TmInt n)) : k)
eval1 ((TmInt m),env,(AddH (TmInt n)):k) = return (TmInt (n + m),tail env,k)

eval1 ((TmNegative e1),env,k)= return (e1,[]:env,(HNegative):k)
eval1 ((TmInt n),env,(HNegative):k) = return (TmInt (-n),tail env,k)

-- Evaluation rules for minus operator
eval1 ((TmSub e1 e2),env,k) = return (e1,[]:env,(HSub e2):k)
eval1 ((TmInt n),env1,(HSub e ):k) = return (e,[]:(tail (env1)),(SubH (TmInt n)) : k)
eval1 ((TmInt m),env,(SubH (TmInt n)):k) = return (TmInt (n - m),tail env,k)

-- Evaluation rules for multiplication operator
eval1 ((TmTimes e1 e2),env,k) = return (e1,[]:env,(HTimes e2):k)
eval1 ((TmInt n),env1,(HTimes e ):k) = return (e,[]:(tail (env1)),(TimesH (TmInt n)) : k)
eval1 ((TmInt m),env,(TimesH (TmInt n)):k) = return (TmInt (n * m),tail env,k)

-- Evaluation rules for division operator
eval1 ((TmDiv e1 e2),env,k) = return (e1,[]:env,(HDiv e2):k)
eval1 ((TmInt n),env1,(HDiv e ):k) = return (e,[]:(tail (env1)),(DivH (TmInt n)) : k)
eval1 ((TmInt m),env,(DivH (TmInt n)):k) = return (TmInt (safeDiv n m),tail env,k)

-- Evaluation rules for projections
eval1 ((TmFst e1),env,k) = return (e1,[]:env, FstH : k)
eval1 ((TmSnd e1),env,k) = return (e1,[]:env, SndH : k)
eval1 ((TmPair v w),env, FstH:k) | isValue v && isValue w = return ( v , tail env, k)
eval1 ((TmPair v w),env, SndH:k) | isValue v && isValue w = return ( w , tail env, k)

-- Evaluation rules for pairs
eval1 ((TmPair e1 e2),env,k) = return (e1,[]:env,(HPair e2):k)
eval1 (v,env1,(HPair e ):k) | isValue v = return(e,[]:(tail (env1)),(PairH v) : k)
eval1 (w,env,(PairH v):k) | isValue w = return ( (TmPair v w),tail env,k)

-- Evaluation rules for if-then-else
eval1 ((TmIf e1 e2 e3),env,k) = return (e1,[]:env,(HIf e2 e3):k)
eval1 (TmTrue,env1,(HIf e2 e3 ):k) = return (e2,tail env1,k)
eval1 (TmFalse,env1,(HIf e2 e3 ):k) = return (e3,tail env1,k)


                                                     

-- Evaluation rules for static variables
{-
eval1 ((TmStatic x typ e1 e2),env,senv,k) = return (e1,env,senv,(HStatic x typ e2 env):k)
eval1 (v,env1,senv,(HStatic x typ e env2):k) | isValue v = do let senv'= update senv x v in do --print( "static " ++ (show v))
                                                                                               return (e, env2,senv' , k)
                                                                                               -}
                                                  
--  Rule to make closures from lambda abstractions.
--TODO : Add type to closure
eval1 ((TmLambda x typ e),env,k) = return ((Cl x typ e env),env, k)

-- Evaluation rules for application
eval1 ((TmApp e1 e2),env,k) = return (e1,[]:env, (HApp e2) : k)
eval1 (v,env,(HApp e ):k ) | isValue v = return (e, []:(tail (env)), (AppH v) : k)
eval1 (v,env,(AppH (Cl x typ e env2) ) : k )  = return (e, localUpdate (tail env) x v, k)



eval1 ((TmAnd e1 e2),env,k) = return (e1,[]:env,(HAnd e2):k)
eval1 ((TmTile e1),env1,(HAnd e ):k) = return (e,[]:(tail (env1)),(AndH (TmTile e1)):k)
eval1 ((TmTile e1),env1,(AndH (TmTile e2)):k) = return (TmTile (andTile e1 e2),tail env1,k)

eval1 ((TmOr e1 e2),env,k) = return (e1,[]:env,(HOr e2):k)
eval1 ((TmTile e1),env1,(HOr e ):k) = return (e,[]:(tail (env1)),(OrH (TmTile e1)):k)
eval1 ((TmTile e1),env1,(OrH (TmTile e2)):k) = return (TmTile (orTile e1 e2),tail env1,k)




eval1 ((TmChange e1 e2 e3 e4),env,k)= return (e1,[]:env,(HChange e2 e3 e4):k)
eval1 (TmInt n ,env1,(HChange e2 e3 e4 ):k) = return (e2,[]:(tail (env1)),(ChHange (TmInt n) e3 e4):k)
eval1 (TmInt m ,env1,(ChHange (TmInt n) e3 e4 ):k) = return (e3,[]:(tail (env1)),(ChanHge (TmInt n) (TmInt m) e4):k)
eval1 (TmInt x ,env1,(ChanHge (TmInt n) (TmInt m) e4 ):k) = return (e4,[]:(tail (env1)),(ChangeH (TmInt n) (TmInt m) (TmInt x)):k)
eval1 (TmTile e1,env1,(ChangeH (TmInt n) (TmInt m) (TmInt x)):k) = return (TmTile (changeTile n m ((show x)!!0) e1 ),tail env1,k)


-- Evaluation rules for replace tile
eval1 ((TmReplace e1 e2 e3 e4),env,k) = return (e1,[]:env,(HReplace e2 e3 e4):k)
eval1 (TmInt n ,env1,(HReplace e2 e3 e4 ):k) = return (e2,[]:(tail (env1)),(ReHplace (TmInt n) e3 e4):k)
eval1 (TmInt m ,env1,(ReHplace (TmInt n) e3 e4 ):k) = return (e3,[]:(tail (env1)),(ReplHace (TmInt n) (TmInt m) e4):k)
eval1 (TmTile x ,env1,(ReplHace (TmInt n) (TmInt m) e4 ):k) = return (e4,[]:(tail (env1)),(ReplaceH (TmInt n) (TmInt m) (TmTile x)):k)
eval1 (TmTile e1,env1,(ReplaceH (TmInt n) (TmInt m) (TmTile x)):k) = do   --print ("replace " ++ (show n) ++ " " ++ (show m) ++ " " ++ (show x) ++ " " ++ (show e1))
                                                                               return (TmTile (replaceTile e1 x n m),tail env1,k)

-- Evaluation rules for inspect tile
eval1 ((TmInspect e1 e2 e3),env,k)  = return (e1,[]:env,(HInspect e2 e3):k)
eval1 (TmInt n ,env1,(HInspect e2 e3 ):k) = return (e2,[]:(tail (env1)),(InHspect (TmInt n) e3):k)
eval1 (TmInt m ,env1,(InHspect (TmInt n) e3 ):k) = return (e3,[]:(tail (env1)),(InspHect (TmInt n) (TmInt m)):k)
eval1 (TmTile e1,env1,(InspHect (TmInt n) (TmInt m)):k) = return (TmInt (inspectTile n m e1),tail env1,k)

-- Eval rules for the funky if statement

eval1 ((TmFunkyIf e1 e2 e3,env ,k)) = return (e1,[]:env,(HFunkyIf e2 e3):k)
eval1 ((TmFalse),env,(HFunkyIf e2 e3):k)= return (e3,(tail (env)),k)
eval1 ((TmTrue),env,(HFunkyIf e2 e3):k)= (funkyIf e2 e3 env k)


-- Eval rules for the funky if else statement
eval1 ((TmFunkyIfElse e1 e2 e3 e4),env,k) = return (e1,[]:env,(HFunkyIfElse e2 e3 e4):k)
eval1 ((TmFalse),env,(HFunkyIfElse e2 e3 e4):k)= funkyIf e3 e4 env k
eval1 ((TmTrue),env,(HFunkyIfElse e2 e3 e4):k)= funkyIf e2 e4 env k

eval1 ((TmFun var params typ exp nextE),env,k) = return (nextE, functionAndParamEnv, k)
    where functionAndParamEnv = localUpdate (updateParams params env (createFuncParam var params 0)) var exp 
          updateParams :: [(String,NapType)] -> Environment -> [(String,Expr)] -> [[(String,Expr)]]
          updateParams [] env _= env
          updateParams (x:xs) env (y:ys) = updateParams xs (localUpdate env (fst y) (TmVar (fst x))) ys


eval1 ((TmApply funcName (TmArgs args)), env, k) = do updatedFunctionEnv <- (updateEnv funcName env args 0)
    -- print ("firstenv " ++ show (env))
    -- print ("secdong part"++show (fst(getValueBinding funcName env env)))
    -- print ("third part" ++ show (updatedFunctionEnv))
                                                      return (fst(getValueBinding funcName env env),  updatedFunctionEnv:env, k)


--   where arg



--Eval rules for a for loop
eval1 ((TmFor e1 e2 e3 e4),env,k) = return (e1,[]:env,(HFor e2 e3 e4):k)
eval1 ((TmInt n ),env1,(HFor e2 e3 e4 ):k) = return (e2,[]:(tail (env1)),(FHor (TmInt n) e3 e4):k)
eval1 ((TmInt m), env1,(FHor (TmInt n) e3 e4 ):k) | n < m = do (e, fEnv) <- for n m (e3,[]:(tail (env1))) e4
                                                               return (e, tail fEnv, k)


eval1 ((TmWhile e1 e2 e3),env,k) = whileLoop e1 e2 e3 env k      


eval1 ((TmReturn e1),env,k) = return (e1,env,k)

eval1 ((TmContinue),env,k) = return (TmUnit,env,k)



-- Evaluation rules for list length
eval1 ((TmLength e1),env,k) = return (e1,[]:env,(HLength):k)
eval1 (TmList e1,env1,(HLength):k) = return (TmInt (length e1),tail env1,k)


--Eval rules for list index
eval1 ((TmIndex e1 e2),env,k) = return (e1,[]:env,(HIndex e2):k)
eval1 (TmList e1,env1,(HIndex e2):k) = return (e2,[]:(tail (env1)),(InHdex (TmList e1)):k)
eval1 (TmInt e2,env1,(InHdex (TmList e1)):k) = return (indexList e1 e2,tail env1,k)

-- Eval rules for list map
eval1 ((TmMap funcName list),env,k) = return (list,[]:env,(HMap funcName ):k)
eval1 (TmList e1,env1,(HMap funcName):k) = do result <- mapList funcName e1 (tail env1)
                                              return (TmList result, tail env1,k)


--Eval rules for the sum of a list
eval1 ((TmSum e1),env,k) = return (e1,[]:env,(HSum):k)
eval1 (TmList e1,env1,(HSum):k) = do result <- sumList e1 (tail env1)
                                     return (TmInt result, tail env1,k)

--Eval rules for the product of a list
eval1 ((TmProd e1),env,k) = return (e1,[]:env,(HProd):k)
eval1 (TmList e1,env1,(HProd):k) = do result <- prodList e1 (tail env1)
                                      return (TmInt result, tail env1,k)


--Eval rules for the hstickall of a list
eval1 ((TmHStickAll e1),env,k) = return (e1,[]:env,(HHStickAll):k)
eval1 (TmList e1,env1,(HHStickAll):k) = do result <- hstickAllList e1 (tail env1)
                                           return (TmTile result, tail env1,k)

--Eval rules for the hstickall of a list
eval1 ((TmVStickAll e1),env,k) = return (e1,[]:env,(HVStickAll):k)
eval1 (TmList e1,env1,(HVStickAll):k) = do result <- vstickAllList e1 (tail env1)
                                           return (TmTile result, tail env1,k)

-- Eval rules for create tile
eval1 ((TmCreate e1),env,k) = return (e1,[]:env,(HCreate):k)
eval1 (TmList e1,env1,(HCreate):k) = do x <- createTile e1 (tail env1)
                                        if checkTile x && checkValues x then return (TmTile x, tail env1,k)
                                        else error "Invalid tile"

--Eval rules for append of list 
eval1 ((TmAppend e1 e2),env,k) = return (e1,[]:env,(HAppend e2):k)
eval1 (TmList e1,env1,(HAppend e2):k) = do y <- evalLoop e2 (tail env1)
                                           return (TmList (e1++[fst y]),tail (env1),k)


--Eval rules for index romve of list 
eval1 ((TmRemove e1 e2),env,k) = return (e1,[]:env,(HRemove e2):k)
eval1 (TmList e1,env1,(HRemove e2):k) = return (e2,[]:(tail (env1)),(RemoveH (TmList e1):) k)
eval1 (TmInt e2,env1,(RemoveH (TmList e1)):k) = return (TmList (removeList e1 e2),tail env1,k)


--Eval rules for modify element of list
eval1 ((TmModify e1 e2 e3),env,k) = return (e1,[]:env,(HModify e2 e3):k)
eval1 ((TmList e1),env1,(HModify e2 e3):k) = return (e2,[]:(tail (env1)),(ModifyH (TmList e1) e3):k)
eval1 ((TmInt e2),env1,(ModifyH (TmList e1) e3):k) =do (e,fEnv) <- evalLoop e3 (tail env1) 
                                                       return (TmList (modifyList e1 e2 e),tail env1,k)

-- Evaluation rules for Let blocks
eval1 ((TmLet x e1 e2),env,k) = return (e1,[]:env,(HLet x e2):k)
eval1 (v,env,(HLet x e ):k) | isValue v = do j<- return (e, update (tail env) x v, k)
                                             return j

-- Evaluation rules for local blocks

eval1 ((TmLocal x typ e1 e2),env,k) = return (e1,[]:env,(HLocal x typ e2):k)
eval1 (v,env,(HLocal x typ e ):k) | isValue v = do j<- return (e, localUpdate (tail env) x v, k)
                                                   return j
                                           


-- Rule for runtime errors
eval1 (e,env,k) = error "Evaluation Error"


stripTmVar :: Expr -> String
stripTmVar (TmVar x) = x

updateEnv:: String -> Environment-> [Expr]-> Int -> IO [(String,Expr)]
updateEnv funcName env[] _ = return []
updateEnv funcName env (x:xs) num = do james <- evalLoop x env
                                       geeth <- updateEnv funcName env xs (num+1)
                                       return ((stripTmVar (fst(getValueBinding (assembleString funcName num) env env)),fst(james)) : geeth)

createFuncParam :: String -> [(String,NapType)] ->Int -> [(String,Expr)]
createFuncParam func_name [] n = []
createFuncParam func_name ((x,t):xs) n = (assembleString func_name n,TmVar x) : createFuncParam func_name xs (n+1)

assembleString :: String  -> Int ->  String 
assembleString str n = str ++ "^" ++ (show n)

modifyList :: [Expr] -> Int -> Expr -> [Expr]
modifyList [] _ _ = []
modifyList (x:xs) n e | n<0 = error "Invalid index: negative number"
                      | n >= length (x:xs) = error "Invalid index: out of bounds"
                      | n == 0 = e : xs
                      | otherwise = x : modifyList xs (n-1) e

removeList :: [Expr] -> Int -> [Expr]
removeList [] _ = []
removeList (x:xs) n | n<0 = error "Invalid index: negative number"
                    | n >= length (x:xs) = error "Invalid index: out of bounds"
                    | n == 0 = xs
                    | otherwise = x : removeList xs (n-1)


funkyIf :: Expr -> Expr -> Environment -> Kontinuation -> IO (Expr, Environment, Kontinuation)
funkyIf e1 e2 env k = do (e, fEnv) <- evalLoop e1 env
                         return (e2, fEnv, k)

whileLoop :: Expr -> Expr -> Expr -> Environment -> Kontinuation -> IO (Expr, Environment, Kontinuation)
whileLoop e1 e2 e3 env k = do
                            (e, fEnv) <- evalLoop e1 env
                            if (isValue e) && (e == TmTrue) then do
                                (e', fEnv') <- evalLoop e2 fEnv
                                whileLoop e1 e2 e3 fEnv' k
                            else if (isValue e) && (e == TmFalse) then do
                                return (e3, fEnv, k)
                            else error "Evaluation Error"


-- Function to iterate the small step reduction to termination
evalLoop :: Expr ->Environment -> IO (Expr,Environment) 
evalLoop e env = evalLoop' (e,env,[])
    where
    evalLoop' :: (Expr, Environment, Kontinuation) ->IO (Expr,Environment)
    evalLoop' (e,env,k )= do 
                            --print ("before eval"++ (show env))
                            (e',env',k') <- eval1 (e,env,k)
                            if (e' == e) && (isValue e') && (null k) 
                              then do --print ("after eval"++ (show senv'))
                                      return (e',env') 
                              else evalLoop' (e',env',k')
                            
for:: Int-> Int-> (Expr,Environment)-> Expr-> IO (Expr,Environment)
for start end (expr,env) body = do
                                    if start > end then return (body,env)
                                    else do
                                         --print senv
                                         (expr',env') <- evalLoop expr env
                                         --print env'
                                         --print start
                                         (for (start+1) end (expr,env') body)
-- Function to unparse underlying values from the AST term
unparse :: Expr -> String 
unparse (TmInt n) = show n
unparse (TmTrue) = "true"
unparse (TmFalse) = "false"
unparse (TmUnit) = "()"
unparse (TmPair e1 e2) = "( " ++ (unparse e1) ++ " , " ++ (unparse e2) ++ " )"
unparse (TmList exprs) = "[" ++ (unparseList exprs) ++ "]"
    where unparseList [] = ""
          unparseList [e] = unparse e
          unparseList (e:es) = unparse e ++ "," ++ unparseList es
unparse (Cl _ _ _ _) = "Function Value"
unparse (TmTile t) = unlines t
unparse (TmCell t) = t
unparse _ = "Unknown"

rotateTile :: Int -> [String] -> [String]
rotateTile 0 t = t
rotateTile n t | n>0 = rotateTile (n-1) (map reverse (transpose t))
               | otherwise = error "Cannot rotate a tile a negative number of times"

hstickTile :: [String] -> [String] -> [String]
hstickTile [] t = t
hstickTile t [] = t
hstickTile (x:xs) (y:ys) | length xs /= length ys = error "Tiles are not the same height" 
                         | otherwise = (x ++ y) : hstickTile xs ys

vstickTile :: [String] -> [String] -> [String]
vstickTile [] t = t
vstickTile t [] = t
vstickTile xs ys | length (xs!!0 )/= length (ys!!0) = error "Tiles are not the same width"
                 | otherwise= xs++ys

hrepeatTile :: Int -> [String] -> [String]
hrepeatTile 0 t = []
hrepeatTile 1 t = t
hrepeatTile n t | n<0 = error "Cannot repeat a tile a negative number of times"
                | otherwise= hstickTile (hrepeatTile (n-1) t) t

vrepeatTile :: Int -> [String] -> [String]
vrepeatTile 0 t = []
vrepeatTile 1 t = t
vrepeatTile n t | n<0 = error "Cannot repeat a tile a negative number of times"
                | otherwise =  vstickTile (vrepeatTile (n-1) t) t

hreflectTile :: [String] -> [String]
hreflectTile [] = []
hreflectTile (x:xs) = (reverse x) : hreflectTile xs

vreflectTile :: [String] -> [String]
vreflectTile [] = []
vreflectTile xs = reverse xs

blankTile :: Int -> [String]
blankTile 0 = []
blankTile n | n < 0 = error "Cannot create a tile of negative size"
            | otherwise = (replicate n (replicate n '0') )

fullTile :: Int -> [String]
fullTile 0 = []
fullTile n | n < 0 = error "Cannot create a tile of negative size"
           |otherwise = (replicate n (replicate n '1') )

scaleTile :: Int -> [String] -> [String]
scaleTile 0 t = []
scaleTile n t | n < 0 = error "Cannot scale a tile by a negative number"
              | otherwise=concat[replicate n (scaleLine n x) | x<-t]

scaleLine :: Int -> String -> String
scaleLine 0 t = []
scaleLine n t = concat[replicate n x | x<-t]

negateTile :: [String] -> [String]
negateTile [] = []
negateTile (x:xs) = (map negateChar x) : negateTile xs

negateChar :: Char -> Char
negateChar '0' = '1'
negateChar '1' = '0'

--TODO check for missing files and remoce .tl extensions

importTile :: String -> IO [String]
importTile s = do x<- readFile (s)
                  return (lines x)

subtileTile :: Int -> Int -> Int -> [String] -> [String]
subtileTile tileSize startX startY  tile | startX < 0 || startY < 0 = error "Cannot start a subtile at a negative position"
                                         | tileSize < 0 = error "Cannot create a subtile of negative size"
                                         | startX + tileSize > length (head tile) || startY + tileSize > length tile = error "Subtile is out of bounds"
                                         |otherwise = subtileRows
    where subtile = take tileSize $ drop startY tile
          subtileRows = map (\row -> take tileSize $ drop startX row) subtile



replaceTile :: [String] -> [String] -> Int -> Int -> [String]
replaceTile baseTile inputTile startX startY | startX < 0 || startY < 0 = error "Cannot start a replace at a negative position"
                                             | startX + length (head inputTile) > length (head baseTile) || startY + length inputTile > length baseTile = error "Replace is out of bounds"
                                             | otherwise = rows
    where endX = startX + length inputTile
          endY = startY + length (head inputTile)
          rows = take startY baseTile ++ zipWith replaceRow [startY..] inputTile ++ drop endY baseTile
          replaceRow y row = take startX (baseTile !! y) ++ row ++ drop endX (baseTile !! y)
    
     

squareTile :: [String] -> [String]
squareTile input = vstickTile top bottom
    where 
        top = hstickTile input (rotateTile 1 input )
        bottom = hstickTile (rotateTile 3 input) (rotateTile 2 input)


andTile :: [String] -> [String] -> [String]
andTile [] [] = []
andTile (x:xs) (y:ys) | length xs /= length ys = error "Tiles are not the same height"
                      | length x /= length y = error "Tiles are not the same width"
                      | otherwise = (andLine x y) : andTile xs ys
    where andLine [] [] = []
          andLine (x:xs) (y:ys) = (andSq x y) : andLine xs ys
          andSq '0' _ = '0'
          andSq _ '0' = '0'
          andSq _ _ = '1'


orTile :: [String] -> [String] -> [String]
orTile [] [] = []
orTile (x:xs) (y:ys) | length xs /= length ys = error "Tiles are not the same height"
                     | length x /= length y = error "Tiles are not the same width"
                     |otherwise = (orLine x y) : orTile xs ys
    where orLine [] [] = []
          orLine (x:xs) (y:ys) = (orSq x y) : orLine xs ys
          orSq '1' _ = '1'
          orSq _ '1' = '1'
          orSq _ _ = '0'

changeTile :: Int -> Int -> Char -> [String] -> [String]
changeTile x y replacement arr | x < 0 || y < 0 = error "Cannot change a tile at a negative position"
                               | x >= length (head arr) || y >= length arr = error "Change is out of bounds"
                               | replacement /= '0' && replacement /= '1' = error "Replacement Character must be 0 or 1"
                               | otherwise =upperRows ++ [newRow] ++ lowerRows
  where (upperRows, row:lowerRows) = splitAt y arr
        (leftCells, _:rightCells) = splitAt x row
        newCell = [replacement]
        newRow = leftCells ++ newCell ++ rightCells

checkTile :: [String] -> Bool
checkTile [] = True
checkTile xs = checkTile' xs (length xs)
    where checkTile' [] _ = True
          checkTile' (x:xs) n | length x /= n = False
                              | otherwise = checkTile' xs n

checkValues :: [String] -> Bool
checkValues [] = True
checkValues (x:xs) = checkValues' x && checkValues xs
    where checkValues' [] = True
          checkValues' (x:xs) | x == '0' || x == '1' = checkValues' xs
                              | otherwise = False

inspectTile :: Int -> Int -> [String] -> Int
inspectTile x y arr | x < 0 || y < 0 = error "Cannot inspect a tile at a negative position"
                    | x >= length (head arr) || y >= length arr = error "Inspection is out of bounds"
                    | otherwise = read [arr!!y!!x] :: Int
  

safeDiv :: Int -> Int -> Int
safeDiv _ 0 = error "Cannot divide by zero"
safeDiv x y = x `div` y

indexList :: [a] -> Int -> a
indexList e1 e2 | e2 >= length e1 = error "Index out of bounds"
                | e2<0 = error "Negative Index"
                | otherwise = e1 !! e2

sumList :: [Expr] -> Environment -> IO Int
sumList [] _ = return 0
sumList (x:xs) env = sumList' (x:xs) env (0)
    where sumList' :: [Expr] -> Environment -> Int -> IO Int
          sumList' [] _ acc = return acc
          sumList' (x:xs) env acc = do z <- evalLoop x env
                                       sumList' xs env (acc + getInt z)


prodList :: [Expr] -> Environment -> IO Int
prodList [] _ = return 1
prodList (x:xs) env = prod' (x:xs) env (1)
    where prod' :: [Expr] -> Environment -> Int -> IO Int
          prod' [] _ acc = return acc
          prod' (x:xs) env acc = do z <- evalLoop x env
                                    prod' xs env (acc * getInt z)


mapList :: String -> [Expr] -> Environment -> IO [Expr]
mapList _ [] _ = return []
mapList funcName (x:xs) env = mapList' funcName (x:xs) env []
    where mapList' :: String -> [Expr] -> Environment -> [Expr] -> IO [Expr]
          mapList' funcName [] _ acc = return acc
          mapList' funcName (x:xs) env acc = do y <- evalLoop x env
                                                z <- evalLoop ( TmApply funcName (TmArgs [(fst (y))])) env
                                                john <- mapList' funcName xs env (acc ++ [fst z])
                                                return (john)

          

getInt:: (Expr, Environment) -> Int
getInt (x, env) = if (isInt x) then (getInt' x) else error "Cannot get int from non-int"
    where 
        isInt :: Expr -> Bool
        isInt (TmInt _) = True
        isInt _ = False

        getInt' :: Expr -> Int
        getInt' (TmInt x) = x

hstickAllList :: [Expr] -> Environment -> IO [String]
hstickAllList [] _ = error "Cannot hstick an empty list"
hstickAllList (x:xs) env = hstickAllList' (x:xs) env []
    where hstickAllList' :: [Expr] -> Environment -> [String] -> IO [String]
          hstickAllList' [] _ acc = return acc
          hstickAllList' (x:xs) env acc = do z <- evalLoop x env
                                             hstickAllList' xs env (hstickTile acc (getTile z))
          getTile :: (Expr, Environment) -> [String]
          getTile (x, env) = if (isTile x) then (getTile' x) else ([])
            where 
                isTile :: Expr -> Bool
                isTile (TmTile _) = True
                isTile _ = False

                getTile' :: Expr -> [String]
                getTile' (TmTile x) = x

vstickAllList :: [Expr] -> Environment -> IO [String]
vstickAllList [] _ = error "Cannot hstick an empty list"
vstickAllList (x:xs) env = hstickAllList' (x:xs) env []
    where hstickAllList' :: [Expr] -> Environment -> [String] -> IO [String]
          hstickAllList' [] _ acc = return acc
          hstickAllList' (x:xs) env acc = do z <- evalLoop x env
                                             hstickAllList' xs env (vstickTile acc (getTile z))
          getTile :: (Expr, Environment) -> [String]
          getTile (x, env) = if (isTile x) then (getTile' x) else ([])
            where 
                isTile :: Expr -> Bool
                isTile (TmTile _) = True
                isTile _ = False

                getTile' :: Expr -> [String]
                getTile' (TmTile x) = x


createTile :: [Expr] -> Environment -> IO [String]
createTile [] _ = error "Cannot create an empty tile"
createTile (x:xs) env = createTile' (x:xs) env [] (length (xs)+1)
    where createTile' :: [Expr] -> Environment -> [String] -> Int -> IO [String]
          createTile' [] _ acc size = return acc
          createTile' (x:xs) env acc size = do z <- evalLoop x env
                                               createTile' xs env (acc ++ [fixLine(show (getTile z)) size]) size
          getTile :: (Expr, Environment) -> Int
          getTile (x, env) = if (isInt x) then (getInt' x) else error "Cannot create a tile with a non-integer value"
            where 
                isInt :: Expr -> Bool
                isInt (TmInt _) = True
                isInt _ = False

                getInt' :: Expr -> Int
                getInt' (TmInt x) = x


argsVar :: String -> String
argsVar e = e ++ "^args"

fixLine :: String ->Int-> String
fixLine xs size | size == length xs = xs
                | otherwise = fixLine ("0"++xs)  size

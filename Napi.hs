import NapTokens
import NapGrammar
import NapTypes
import NapEval
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do putStrLn ("Nap Interactive Mode - enter an expression : ")
           sourceText <- getLine
           let lexedpro = alexScanTokens sourceText
           putStrLn ("Lexed as " ++ (show lexedpro) ++ "\n")
           let parsedProg = parseCalc (alexScanTokens sourceText)
           putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")
           putStrLn ("Type Checking : " ++ (show parsedProg) ++ "\n")
           let typedProg = typeOf [] parsedProg
           putStrLn ("Type Checking Passed with type " ++ (unparseType (fst typedProg)) ++ "\n")
           result <- evalLoop parsedProg [[]]
           putStrLn ("Evaluates to \n" ++ (unparse (fst result)) ++ "\n")
           putStrLn ("With environment \n " ++ (show (snd result)) ++ "\n")
           main'

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               putStrLn("----------------")
               hPutStrLn stderr err
               putStrLn("----------------")
               main



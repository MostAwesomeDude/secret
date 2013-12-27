module Main where

import System.Environment
import Text.Parsec.Prim
import Text.PrettyPrint.ANSI.Leijen

import Expander
import Expression
import Parser
import Printer ()
import Simplifier

parser :: Parsec Token () Expr
parser = expr

main :: IO ()
main = do
    [filename] <- getArgs
    s <- readFile filename
    let tokens = alexScanTokens s
        result = parse parser filename tokens
    case result of
        Left err  -> print err
        Right ast -> do
            putStrLn "Parsed:"
            print ast
            putStrLn "Formatted:"
            print $ pretty ast
            let expanded = expand ast
            putStrLn "Expanded:"
            print $ pretty expanded
            let simplified = simplify expanded
            putStrLn "Simplified:"
            print $ pretty simplified

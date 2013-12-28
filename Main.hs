module Main where

import Control.Lens
import System.Environment
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.PrettyPrint.ANSI.Leijen

import Expander
import Expression
import Lexer
import Parser
import Printer ()
import Simplifier

parser :: Parsec [(SourcePos, Token)] () Expr
parser = fullExpr

convertPos :: String -> [(AlexPosn, Token)] -> [(SourcePos, Token)]
convertPos name = map $ _1 %~ (\(AlexPn _ l c) -> newPos name l c)

main :: IO ()
main = do
    [filename] <- getArgs
    s <- readFile filename
    let tokens = alexScanTokens s
    print tokens
    let result = parse parser filename (convertPos filename tokens)
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

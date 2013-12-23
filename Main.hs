module Main where

import System.Environment
import Text.Parser.Token
import Text.PrettyPrint.ANSI.Leijen
import Text.Trifecta.Parser

import Expander
import Expression
import Parser
import Printer ()

parser :: Unlined Parser Expr
parser = expr

main :: IO ()
main = do
    [filename] <- getArgs
    result <- parseFromFile (runUnlined parser) filename
    case result of
        Nothing -> return ()
        Just ast -> do
            putStrLn "Parsed:"
            print ast
            putStrLn "Formatted:"
            print $ pretty ast
            let expanded = expand ast
            putStrLn "Expanded:"
            print $ pretty expanded
            putStrLn "Simplified:"
            print $ pretty expanded

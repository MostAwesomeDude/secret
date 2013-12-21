module Main where

import System.Environment
import Text.PrettyPrint.ANSI.Leijen
import Text.Trifecta.Parser

import Expander
import Parser
import Printer

main :: IO ()
main = do
    [filename] <- getArgs
    result <- parseFromFile expr filename
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

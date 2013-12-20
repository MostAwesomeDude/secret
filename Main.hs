module Main where

import System.Environment
import Text.PrettyPrint.ANSI.Leijen
import Text.Trifecta.Parser

import Parser
import Printer

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
            putStrLn "Expanded:"
            print $ pretty ast
            putStrLn "Simplified:"
            print $ pretty ast

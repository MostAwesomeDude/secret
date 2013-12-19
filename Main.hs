module Main where

import System.Environment
import Text.Trifecta.Parser

import Parser

main = do
    [filename] <- getArgs
    result <- parseFromFile expr filename
    case result of
        Nothing -> return ()
        Just ast -> do
            putStrLn "Parsed:"
            print ast
            putStrLn "Formatted:"
            putStrLn $ formatExpr ast
            putStrLn "Expanded:"
            putStrLn $ formatExpr ast
            putStrLn "Simplified:"
            putStrLn $ formatExpr ast

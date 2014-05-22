-- Copyright (C) 2014 Google Inc. All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License. You may obtain a copy
-- of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
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

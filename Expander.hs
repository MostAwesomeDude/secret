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
module Expander where

import Control.Lens

import Expression

makeCall :: Expr -> String -> [Expr] -> Expr
makeCall obj meth = Arguments . ECall obj . NounExpr $ Noun meth

wrapFunctions :: Expr -> Expr
wrapFunctions = transform $ \expr -> case expr of
    (Function name ps rv body) -> Function name ps rv $ Escape (Noun "return") body
    e -> e

elimAug :: Expr -> Expr
elimAug = transform $ \expr -> case expr of
    (Augmented o target val) -> Assignment target $ Binary o target val
    e -> e

flipCompares :: Expr -> Expr
flipCompares = transform $ \expr -> case expr of
    (Comparison Different    obj other)
        -> Unary Not $ Comparison Equal obj other
    (Comparison DoesNotMatch obj other)
        -> Unary Not $ Comparison Matches obj other
    e -> e

elimShiftRight :: Expr -> Expr
elimShiftRight = transform $ \expr -> case expr of
    (Binary ShiftRight obj offset)
        -> Binary ShiftLeft obj $ Unary Negate offset
    e -> e

elimUnary :: Expr -> Expr
elimUnary = transform $ \expr -> case expr of
    (Unary o obj) -> makeCall obj (method o) []
    e -> e
    where
    method Complement = "complement"
    method Negate = "negate"    
    method Not = "not"

elimRange :: Expr -> Expr
elimRange = transform $ \expr -> case expr of
    (Range i l r) ->
        makeCall (NounExpr (Noun "__makeOrderedSpace")) (method i) [l, r]
    e -> e
    where
    method Through = "op__thru"
    method Till = "op__till"

elimCmp :: Expr -> Expr
elimCmp = transform $ \expr -> case expr of
    (Comparison GTEQ obj other) ->
        makeCall (makeCall obj "compareTo" [other]) "atLeastZero" []
    (Comparison GreaterThan obj other) ->
        makeCall (makeCall obj "compareTo" [other]) "aboveZero" []
    (Comparison LTEQ obj other) ->
        makeCall (makeCall obj "compareTo" [other]) "atMostZero" []
    (Comparison LessThan obj other) ->
        makeCall (makeCall obj "compareTo" [other]) "belowZero" []
    (Comparison Magnitude obj other) ->
        makeCall (makeCall obj "compareTo" [other]) "isZero" []
    e -> e

elimEq :: Expr -> Expr
elimEq = transform $ \expr -> case expr of
    (Comparison Equal obj other) ->
        makeCall (NounExpr (Noun "__equalizer")) "sameEver" [obj, other]
    e -> e

elimBinary :: Expr -> Expr
elimBinary = transform $ \expr -> case expr of
    (Binary o obj other) -> makeCall obj (method o) [other]
    e -> e
    where
    method Add = "add"
    method BitAnd = "and"
    method BitOr = "or"
    method BitXor = "xor"
    method Divide = "approxDivide"
    method FloorDivide = "floorDivide"
    method Modulus = "modulo"
    method Multiply = "multiply"
    method Power = "pow"
    method Remainder = "remainder"
    method ShiftLeft = "shiftLeft"
    method ShiftRight = error "ShiftRight cannot be lowered"
    method Subtract = "subtract"

expand :: Expr -> Expr
expand = foldl (.) id
    [ wrapFunctions
    , elimAug
    , flipCompares
    , elimShiftRight
    , elimUnary
    , elimRange
    , elimCmp
    , elimEq
    , elimBinary ]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
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

module Expression where

import Control.Lens
import Data.Data

data Literal = Null
             | EInteger Integer
             | EFloat Double
             | EChar Char
             | EString String
             | EURI String
    deriving (Data, Eq, Show, Typeable)

makeLenses ''Literal

data Noun = Noun String
          | QLHole Integer
          | QPHole Integer
    deriving (Data, Eq, Show, Typeable)

makeLenses ''Noun

data UOp = Complement | Negate | Not
    deriving (Data, Enum, Eq, Ord, Show, Typeable)

makeLenses ''UOp

data BOp = Add | BitAnd | BitOr | BitXor | Divide | FloorDivide | Modulus
         | Multiply | Power | Remainder | ShiftLeft | ShiftRight | Subtract
    deriving (Data, Enum, Eq, Ord, Show, Typeable)

makeLenses ''BOp

data COp = Different | Equal | GTEQ | GreaterThan | LTEQ | LessThan
         | Magnitude | Matches | DoesNotMatch
    deriving (Data, Enum, Eq, Ord, Show, Typeable)

makeLenses ''COp

data Interval = Through | Till
    deriving (Data, Enum, Eq, Ord, Show, Typeable)

makeLenses ''Interval

data Exit = Break | Continue | Return
    deriving (Data, Enum, Eq, Ord, Show, Typeable)

makeLenses ''Exit

data Pattern = SuchThat Pattern Expr
             | PList [Pattern]
             | PListAnd [Pattern] Pattern
             | ExactMatch Expr
             | Namer Noun Expr
             | Varying Pattern
             | Final Noun
    deriving (Data, Eq, Show, Typeable)

data Expr = LitExpr Literal
          | NounExpr Noun
          | Unary UOp Expr
          | Binary BOp Expr Expr
          | Comparison COp Expr Expr
          | Range Interval Expr Expr
          | Or Expr Expr
          | And Expr Expr
          | Quasi String String
          | EList [Expr]
          | EMap [(Expr, Expr)]
          | Scope Expr
          | Sequence Expr Expr
          | Augmented BOp Expr Expr
          | Assignment Expr Expr
          | Define Pattern Expr
          | Function Noun [Pattern] Expr Expr
          | Method Noun [Pattern] Expr Expr
          | If Expr Expr Expr
          | Switch Expr [(Pattern, Expr)]
          | Try Expr [(Pattern, Expr)]
          | TryFinally Expr [(Pattern, Expr)] Expr
          | Escape Noun Expr
          | While Expr Expr
          | For Expr Expr Expr Expr
          | Arguments Expr [Expr]
          | Index Expr [Expr]
          | EProperty Expr Expr
          | ECall Expr Expr
          | ESend Expr Expr
          | EjectExit Exit Expr
          | Object Noun [Expr] (Maybe (Pattern, Expr))
    deriving (Data, Eq, Show, Typeable)

instance Plated Expr

-- Make Pattern lenses here, since Pattern and Expr have a circular
-- dependency.
makeLenses ''Pattern
makeLenses ''Expr

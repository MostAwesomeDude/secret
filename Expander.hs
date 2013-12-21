module Expander where

import Control.Lens

import Expression

makeCall :: Expr -> String -> [Expr] -> Expr
makeCall obj meth = Arguments . Call obj . NounExpr $ Noun meth

elimUnary :: Expr -> Expr
elimUnary = transform $ \expr -> case expr of
    (Unary o obj) -> makeCall obj (method o) []
    e -> e
    where
    method Complement = "complement"
    method Negate = "negate"    
    method Not = "not"

elimDiff :: Expr -> Expr
elimDiff = transform $ \expr -> case expr of
    (Comparison Different obj other) -> Unary Not $ Comparison Equal obj other
    e -> e

expand :: Expr -> Expr
expand = foldl (.) id
    [ elimDiff
    , elimUnary ]

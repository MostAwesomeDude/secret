module Expander where

import Control.Lens

import Expression

makeCall :: Expr -> String -> [Expr] -> Expr
makeCall obj meth = Arguments . Call obj . NounExpr $ Noun meth

wrapFunctions :: Expr -> Expr
wrapFunctions = transform $ \expr -> case expr of
    (Function name ps rv body) -> Function name ps rv $ Escape (Noun "return") body
    e -> e

flipCompares :: Expr -> Expr
flipCompares = transform $ \expr -> case expr of
    (Comparison Different obj other) -> Unary Not $ Comparison Equal obj other
    (Comparison NoMatch   obj other) -> Unary Not $ Comparison Match obj other
    e -> e

elimUnary :: Expr -> Expr
elimUnary = transform $ \expr -> case expr of
    (Unary o obj) -> makeCall obj (method o) []
    e -> e
    where
    method Complement = "complement"
    method Negate = "negate"    
    method Not = "not"

elimEq :: Expr -> Expr
elimEq = transform $ \expr -> case expr of
    (Comparison Equal obj other) ->
        makeCall (NounExpr (Noun "__equalizer")) "sameEver" [obj, other]
    e -> e

expand :: Expr -> Expr
expand = foldl (.) id
    [ wrapFunctions
    , flipCompares
    , elimUnary
    , elimEq ]

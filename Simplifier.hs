module Simplifier where

import Control.Lens

import Expression

pruneNulls :: Expr -> Expr
pruneNulls = transform $ \expr -> case expr of
    (Sequence (LitExpr Null) inner) -> inner
    (Sequence inner (LitExpr Null)) -> inner
    e -> e

simplify :: Expr -> Expr
simplify = foldl (.) id
    [ pruneNulls ]

{-# LANGUAGE DeriveFunctor #-}

module Interpreter where

import Control.Monad.Free

data Inst a = Dup a
            | Rot a
            | Pop a
            | Swap a
            | GetSlot Int a
            | PutSlot Int a
    deriving (Eq, Functor, Show)

data Slot = Slot | NoSlot
    deriving (Eq, Show)

data Object = SlotObj Slot
    deriving (Eq, Show)

data Environment a = Env
                   { slots :: [Slot]
                   , stack :: [a]
                   }
    deriving (Eq, Show)

dup :: Free Inst ()
dup = liftF $ Dup ()

pop :: Free Inst ()
pop = liftF $ Pop ()

swap :: Free Inst ()
swap = liftF $ Swap ()

eval :: Free Inst () -> Environment Object -> Environment Object
eval inst env = go env inst
    where
    go env (Free action) = go' env action
    go env _ = env
    go' env@(Env {stack = (a:as)}) (Dup cont) = go (env {stack = (a:a:as) }) cont
    go' env@(Env {stack = (a:b:c:as)}) (Rot cont) = go (env {stack = (b:c:a:as) }) cont
    go' env@(Env {stack = (a:as)}) (Pop cont) = go (env {stack = as }) cont
    go' env@(Env {stack = (a:b:as)}) (Swap cont) = go (env {stack = (b:a:as) }) cont
    go' (Env ss st) (GetSlot i cont) = let s = ss !! i in
        go (Env ss (SlotObj s:st)) cont
    go' (Env ss (SlotObj s:st)) (PutSlot i cont) = let ss' = take i ss ++ (s:drop (i+1) ss) in
        go (Env ss' st) cont
    go' env inst = error $ "Stack error: " ++ show inst ++ " Stack: " ++ show env

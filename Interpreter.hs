{-# LANGUAGE DeriveFunctor #-}

module Interpreter where

import Control.Monad.Free

data Inst a = Dup a
            | Rot a
            | Pop a
            | Swap a
    deriving (Eq, Functor, Show)

dup :: Free Inst ()
dup = liftF $ Dup ()

pop :: Free Inst ()
pop = liftF $ Pop ()

eval :: (Num a, Show a) => Free Inst () -> [a]
eval = help [42]
    where
    help as (Free action) = go as action
    help as _ = as
    go (a:as) (Dup cont) = help (a:a:as) cont
    go (a:b:c:as) (Rot cont) = help (b:c:a:as) cont
    go (a:as) (Pop cont) = help as cont
    go (a:b:as) (Swap cont) = help (b:a:as) cont
    go stack inst = error $ "Stack error: " ++ show inst ++ " Stack: " ++ show stack

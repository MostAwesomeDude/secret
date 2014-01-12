{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import Control.Lens
import Control.Monad.Free
import Control.Monad.State

data Inst a = Dup a
            | Rot a
            | Pop a
            | Swap a
            | GetSlot Int a
            | PutSlot Int a
            | Literal Int a
    deriving (Eq, Functor, Show)

data Slot = Slot | NoSlot
    deriving (Eq, Show)

data Object = SlotObj Slot
            | StrObj String
            | IntObj Integer
    deriving (Eq, Show)

data Environment a = Env
                   { _literals :: [Object]
                   , _slots :: [Slot]
                   , _stack :: [a]
                   }
    deriving (Eq, Show)

makeLenses ''Environment

dup :: Free Inst ()
dup = liftF $ Dup ()

pop :: Free Inst ()
pop = liftF $ Pop ()

swap :: Free Inst ()
swap = liftF $ Swap ()

eval :: Free Inst () -> Environment Object -> Environment Object
eval inst env = flip execState env $ iterM go inst
    where
    go (Dup ma) = stack %= (\(x:xs) -> x:x:xs) >> ma
    go (Rot ma) = stack %= (\(x:y:z:xs) -> y:z:x:xs) >> ma
    go (Pop ma) = stack %= (\(x:xs) -> xs) >> ma
    go (Swap ma) = stack %= (\(x:y:xs) -> y:x:xs) >> ma
    go (GetSlot i ma) = do
        Just s <- preuse $ slots . ix i
        stack %= (SlotObj s:)
        ma
    go (PutSlot i ma) = do
        SlotObj s <- uses stack head
        slots . ix i .= s
        ma
    go (Literal i ma) = do
        Just l <- preuse $ literals . ix i
        stack %= (l:)
        ma

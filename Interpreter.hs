{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import Control.Lens
import Control.Monad.Free
import Control.Monad.State
import Prelude hiding (drop)

data Inst a = Dup a
            | Rot a
            | Drop a
            | Swap a
            | GetSlot Int a
            | PutSlot Int a
            | Literal Int a
            | BindObject Int Int a
            | Call String Int a
    deriving (Eq, Functor, Show)

type Insts = Free Inst ()

data Script = Script [(String, Int, Insts)]
    deriving (Eq, Show)

data Object = ScriptObj Script Frame
            | SlotObj (Maybe Object)
            | StrObj String
            | IntObj Integer
    deriving (Eq, Show)

data Frame = Frame
           { _scripts :: [(Script, Int)]
           , _literals :: [Object]
           , _slots :: [Maybe Object]
           }
    deriving (Eq, Show)

data Environment a = Env
                   { _frame :: Frame
                   , _stack :: [a]
                   }
    deriving (Eq, Show)

makeLenses ''Environment
makeLenses ''Frame

findScript :: [(String, Int, Insts)] -> String -> Int -> Maybe Insts
findScript ((verb', arity', insts):ss) verb arity
    | verb == verb' && arity == arity' = Just insts
    | otherwise = findScript ss verb arity
findScript [] _ _ = Nothing

call :: Object -> String -> Int -> [Object] -> State (Environment Object) ()
call (ScriptObj (Script ss) frame) verb arity args =
    case findScript ss verb arity of
        Just insts -> modify $ eval insts
        Nothing    -> fail "Method not found"
call (IntObj i) verb arity args = push $ intCall i verb arity args
    where push obj = stack %= (obj:)

intCall :: Integer -> String -> Int -> [Object] -> Object
intCall i "mul" 1 [IntObj j] = IntObj $ i * j

dup :: Free Inst ()
dup = liftF $ Dup ()

drop :: Free Inst ()
drop = liftF $ Drop ()

swap :: Free Inst ()
swap = liftF $ Swap ()

eval :: Free Inst () -> Environment Object -> Environment Object
eval inst env = flip execState env $ iterM go inst
    where
    go (Dup ma) = stack %= (\(x:xs) -> x:x:xs) >> ma
    go (Rot ma) = stack %= (\(x:y:z:xs) -> y:z:x:xs) >> ma
    go (Drop ma) = stack %= (\(x:xs) -> xs) >> ma
    go (Swap ma) = stack %= (\(x:y:xs) -> y:x:xs) >> ma
    go (GetSlot i ma) = do
        Just s <- preuse $ frame . slots . ix i
        stack %= (SlotObj s:)
        ma
    go (PutSlot i ma) = do
        (SlotObj s:st) <- use stack
        stack .= st
        frame . slots . ix i .= s
        ma
    go (Literal i ma) = do
        Just l <- preuse $ frame . literals . ix i
        stack %= (l:)
        ma
    go (BindObject scriptIx slotIx ma) = do
        Just (script, len) <- preuse $ frame . scripts . ix scriptIx
        st <- use stack
        let (auditors, st') = splitAt len st
            (vars, st'')    = splitAt len st'
            obj = ScriptObj script (Frame [] vars [])
        stack .= obj:st''
        frame . slots . ix slotIx .= Just obj
        ma
    go (Call verb arity ma) = do
        obj:st <- use stack
        let (args, st') = splitAt arity st
        stack .= st'
        call obj verb arity args
        ma

test :: Free Inst ()
test = do
    liftF $ Literal 0 ()
    liftF $ Literal 1 ()
    liftF $ Call "mul" 1 ()

test' :: Free Inst ()
test' = do
    liftF $ Literal 0 ()
    liftF $ Literal 1 ()
    liftF $ BindObject 0 0 ()
    liftF $ Call "run" 2 ()

mulScript :: Script
mulScript = Script [("run", 2, test)]

main :: IO ()
main = print env'
    where
    env = Env (Frame [(mulScript, 0)] [IntObj 6, IntObj 7] []) []
    env' = eval test' env

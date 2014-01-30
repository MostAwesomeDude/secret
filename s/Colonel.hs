{-# LANGUAGE TemplateHaskell #-}

module Colonel where

import Control.Lens hiding (Context)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import qualified Data.Map as M

type Name = String
type Verb = String

data Pattern = Binding Expr Expr
             | Final Expr Expr
             | Ignore Expr
             | List [Pattern] Expr
             | Var Expr Expr
             | Via Expr Pattern
    deriving (Eq, Show)

data Script = Script Verb Expr [Expr] [Expr] [Expr] -- should be Method and Matcher
    deriving (Eq, Show)

data Frame = Frame { _slots :: M.Map Name Object }
    deriving (Eq, Show)

data Object = ScriptObj Script Frame
            | BoolObj Bool
            | FloatObj Double
            | IntObj Integer
            | StrObj String
            | NullObj
    deriving (Eq, Show)

data Expr = Assign Expr Expr
          | BindingExpr Expr
          | Call Expr Verb [Expr]
          | Catch Pattern Expr
          | Def Pattern Expr Expr
          | Escape Pattern Expr Expr -- should be Catch
          | Finally Expr
          | If Expr Expr Expr
          | ObjExpr Object
          | Matcher Pattern Expr
          | Method Verb String [Pattern] Expr Expr
          | Noun Name
          | Object Name Pattern Script
          | Sequence [Expr]
          | Try Expr Pattern Expr
    deriving (Eq, Show)

makeLenses ''Frame

type Context a = EitherT String (State Frame) a

null :: Expr
null = ObjExpr NullObj

throw :: String -> Context a
throw = left

applyPattern :: Pattern -> Context Object
applyPattern (Ignore expr) = eval expr
applyPattern (Via expr patt) = do
    obj <- applyPattern patt
    eval $ Call expr "run" [ObjExpr obj]

eval :: Expr -> Context Object
eval (ObjExpr obj) = return obj
eval (If cond cons alt) = do
    cond' <- eval cond
    case cond' of
        BoolObj True  -> eval cons
        BoolObj False -> eval alt
        _             -> throw "Non-boolean in conditional"
eval (Call obj verb args) = do
    obj' <- eval obj
    args' <- mapM eval args
    case (obj', verb, args') of
        (IntObj i, "mul", [IntObj i']) -> return . IntObj $ i * i'
        _                              -> throw "Invalid verb/arity"
eval (Noun name) = do
    slots <- lift $ gets _slots
    case M.lookup name slots of
        Just obj -> return obj
        Nothing  -> throw "Name not in scope"
eval (Sequence exprs) = do
    exprs' <- mapM eval exprs
    return . last $ exprs'
eval (Def patt target expr) = do
    rvalue <- eval expr
    case patt of
        Final (Noun name) _ -> slots . ix name .= rvalue
        Var (Noun name) _ -> slots . ix name .= rvalue
    return rvalue

runContext :: Context Object -> Either String Object
runContext c = evalState (runEitherT c) (Frame M.empty)

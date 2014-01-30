{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Colonel where

import Control.Monad.Free
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import qualified Data.Map as M

type Name = String

data Pattern a = Binding a a
               | Final a a
               | Ignore a
               | List [Pattern a] a
               | Var a a
               | Via a (Pattern a)
    deriving (Eq, Functor, Show)

data Script a = Script String a [a] [Expr a] [Expr a] -- should be Method and Matcher
    deriving (Eq, Functor, Show)

data Literal = LBool Bool
             | LFloat Double
             | LInt Integer
             | LString String
    deriving (Eq, Show)

data Expr a = Assign a a
            | BindingExpr a
            | Call a String [a]
            | Catch (Pattern a) a
            | Def (Pattern a) a a
            | Escape (Pattern a) a (Expr a) -- should be Catch
            | Finally a
            | If a a a
            | LiteralExpr Literal
            | Matcher (Pattern a) a
            | Method String String [Pattern a] a a
            | Noun Name
            | Object Name (Pattern a) (Script a)
            | Sequence [a]
            | Try a (Pattern a) a
    deriving (Eq, Functor, Show)

data Object a = ScriptObj (Script a) (Frame a)
              | BoolObj Bool
              | FloatObj Double
              | IntObj Integer
              | StrObj String
    deriving (Eq, Functor, Show)

data Frame a = Frame { _slots :: M.Map Name a }
    deriving (Eq, Functor, Show)

type Context a = EitherT String (State (Frame a)) a

throw :: String -> Context a
throw = left

litToObj :: Literal -> Object a
litToObj (LBool b) = BoolObj b
litToObj (LFloat d) = FloatObj d
litToObj (LInt i) = IntObj i
litToObj (LString s) = StrObj s

eval :: Free Expr (Object a) -> Context (Object a)
eval (Pure obj) = return obj
eval (Free (LiteralExpr l)) = return . litToObj $ l
eval (Free (If cond cons alt)) = do
    cond' <- eval cond
    case cond' of
        BoolObj True  -> eval cons
        BoolObj False -> eval alt
        _             -> throw "Non-boolean in conditional"
eval (Free (Call obj verb args)) = do
    obj' <- eval obj
    args' <- mapM eval args
    case (obj', verb, args') of
        (IntObj i, "mul", [IntObj i']) -> return . IntObj $ i * i'
        _                              -> throw "Invalid verb/arity"
eval (Free (Noun name)) = do
    slots <- lift $ gets _slots
    case M.lookup name slots of
        Just obj -> return obj
        Nothing  -> throw "Name not in scope"
eval (Free (Sequence exprs)) = do
    exprs' <- mapM eval exprs
    return . last $ exprs'

runContext :: Context (Object a) -> Either String (Object a)
runContext c = evalState (runEitherT c) (Frame M.empty)

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Colonel where

import Control.Monad.Free
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

data Frame a = Frame { _slots :: M.Map Name (Object a) }
    deriving (Eq, Functor, Show)

litToObj :: Literal -> Object a
litToObj (LBool b) = BoolObj b
litToObj (LFloat d) = FloatObj d
litToObj (LInt i) = IntObj i
litToObj (LString s) = StrObj s

eval :: Frame (Object a) -> Free Expr (Object a) -> Either String (Object a)
eval _ (Pure obj) = Right obj
eval _ (Free (LiteralExpr l)) = return . litToObj $ l
eval frame (Free (If cond cons alt)) = do
    cond' <- eval frame cond
    case cond' of
        BoolObj True  -> eval frame cons
        BoolObj False -> eval frame alt
        _             -> Left "Non-boolean in conditional"
eval frame (Free (Call obj verb args)) = do
    obj' <- eval frame obj
    args' <- mapM (eval frame) args
    case (obj', verb, args') of
        (IntObj i, "mul", [IntObj i']) -> Right . IntObj $ i * i'
        _                              -> Left "Invalid verb/arity"

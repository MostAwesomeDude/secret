module Parser where

import Control.Applicative
import Control.Monad
import qualified Data.HashSet as HS
import Text.Parsec.Pos
import Text.Parsec.Prim (Parsec(..), token)
import Text.Parser.Combinators
import Text.Parser.Expression

import Expression
import Lexer

type P = Parsec [Token] ()

tok :: (Token -> Maybe a) -> P a
tok = token show (\_ -> newPos "" 0 0)

sat :: (Token -> Bool) -> P Token
sat f = tok (\t -> if f t then Just t else Nothing)

exact :: Token -> P Token
exact t = sat (t ==)

assoc :: [(Token, a)] -> P a
assoc ts = tok (\t -> search t ts)
    where
    search t [] = Nothing
    search t ((t', a):ts)
        | t == t' = Just a
        | otherwise = search t ts

literal :: P Literal
literal = tok go <?> "literal"
    where
    go (TChar c) = Just $ EChar c
    go (TInt i) = Just $ EInteger i
    go (TFloat f) = Just $ EFloat f
    go (TURI u) = Just $ EURI u
    go _ = Nothing

identifier :: P String
identifier = tok go
    where
    go (TIdentifier s) = Just s
    go _ = Nothing

noun :: P Noun
noun = tok go
    where
    go (TIdentifier s) = Just $ Noun s
    go _ = Nothing
   -- <|> QLHole <$> (char '$' *> braces natural)
   -- <|> QPHole <$> (char '@' *> braces natural)

interval :: P Interval
interval = tok go <?> "interval"
    where
    go (TInterval i) = Just i
    go _ = Nothing

exit :: P Exit
exit = assoc [(TBreak, Break), (TContinue, Continue), (TReturn, Return)]

brackets :: P a -> P a
brackets = between (exact OpenBracket) (exact CloseBracket)

braces :: P a -> P a
braces = between (exact OpenBrace) (exact CloseBrace)

parens :: P a -> P a
parens = between (exact OpenParen) (exact CloseParen)

listOf :: P a -> P [a]
listOf p = brackets $ sepBy p (exact Comma)

pListAnd :: P Pattern
pListAnd = do
    ps <- listOf pattern
    exact $ TBinary Add
    p <- pattern
    return $ PListAnd ps p

namer :: P Pattern
namer = Namer <$> noun <* exact Colon <*> expr

pattern :: P Pattern
pattern = choice
    [ Varying <$> (exact TVar *> pattern)
    , try $ pListAnd
    , PList <$> listOf pattern
    , ExactMatch <$> (exact (TCompare Equal) *> expr)
    , try namer
    , Final <$> noun ]
    <?> "pattern"

quasi :: P String
quasi = tok go <?> "quasiquote"
    where
    go (TQuasi s) = Just s
    go _ = Nothing

mapPair :: P (Expr, Expr)
mapPair = do
    k <- expr
    exact Pair
    v <- expr
    return (k, v)
    <?> "pair"

ifExpr :: P Expr
ifExpr = do
    exact TIf
    cond <- parens expr
    t <- braces expr
    mf <- optional $ do
        exact TElse
        braces expr <|> ifExpr
    return $ case mf of
        Just f  -> If cond t f
        Nothing -> If cond t $ LitExpr Null

matchExpr :: P (Pattern, Expr)
matchExpr = do
    exact TMatch
    p <- pattern
    clause <- braces expr
    return (p, clause)

switchExpr :: P Expr
switchExpr = do
    exact TSwitch
    switch <- parens expr
    cases <- many matchExpr
    return $ Switch switch cases

tryExpr :: P Expr
tryExpr = do
    exact TTry
    action <- braces expr
    catches <- many $ do
        exact TCatch
        p <- pattern
        clause <- braces expr
        return (p, clause)
    mfinally <- optional $ exact TFinally *> braces expr
    return $ case mfinally of
        Nothing      -> Try action catches
        Just finally -> TryFinally action catches finally

forExpr :: P Expr
forExpr = do
    exact TFor
    (kpattern, vpattern) <- mapPair
    exact TIn
    cexpr <- expr
    bexpr <- braces expr
    return $ For kpattern vpattern cexpr bexpr

exitExpr :: P Expr
exitExpr = EjectExit <$> exit <*> expr <?> "exit"

parensOf :: P a -> P [a]
parensOf p = parens $ sepBy p (exact Comma)

methodExpr :: P Expr
methodExpr = do
    exact TTo
    name <- noun
    ps <- parensOf pattern <|> pure []
    -- Guard on the return value.
    rv <- exact Colon *> expr
    body <- expr
    return $ Function name ps rv body

defineExpr :: P Expr
defineExpr = do
    exact TDef
    try define <|> funcobj
    where
    define = do
        binding <- pattern
        exact Assign
        body <- expr
        return $ Define binding body
    funcobj = do
        n <- noun
        object n <|> function n
    function name = do
        ps <- parensOf pattern
        -- Guard on the return value.
        exact Colon
        rv <- expr
        body <- expr
        return $ Function name ps rv body
    object name = braces $ do
        methods <- many $ methodExpr
        match <- optional matchExpr
        return $ Object name methods match

term :: P Expr
term = choice
    [ LitExpr <$> literal
    , parens expr
    , Scope <$> braces expr
    , try $ EMap <$> brackets (sepBy mapPair (exact Comma))
    , EList <$> listOf expr
    , ifExpr
    , switchExpr
    , tryExpr
    , forExpr
    , exitExpr
    , exact TEscape *> pure Escape <*> noun <*> braces expr
    , exact TWhile *> pure While <*> parens expr <*> braces expr
    , defineExpr
    , Quasi "simple" <$> quasi
    , try $ Quasi <$> identifier <*> quasi
    , NounExpr <$> noun ]
    <?> "primitive expression"

bin :: (a -> a -> a) -> Token -> Assoc -> Operator P a
bin cons t = Infix (exact t *> pure cons)

binary :: BOp -> Token -> Operator P Expr
binary op t = bin (Binary op) t AssocLeft

comparison :: COp -> Token -> Operator P Expr
comparison op t = bin (Comparison op) t AssocNone

pre :: Token -> UOp -> Operator P Expr
pre t op = Prefix (exact t *> pure (Unary op))

augmented :: Token -> Maybe BOp
augmented (TAugmented op) = Just op
augmented _ = Nothing

table :: OperatorTable P Expr
table = [ [ Postfix (flip Arguments <$> parensOf expr) ]
        , [ bin ECall Dot AssocLeft ]
        , [ bin ESend Send AssocLeft
          , bin EProperty Property AssocLeft
          , Postfix (flip Index <$> listOf expr) ]
        , [ pre (TUnary Not) Not
          , pre (TUnary Complement) Complement
          , pre (TUnary Negate) Negate ]
        , [ binary Power (TBinary Power) ]
        , [ binary Multiply (TBinary Multiply)
          , binary FloorDivide (TBinary FloorDivide)
          , binary Divide (TBinary Divide)
          , binary Modulus (TBinary Modulus)
          , binary Remainder (TBinary Remainder) ]
        , [ binary Add (TBinary Add), binary Subtract (TBinary Subtract) ]
        , [ binary ShiftLeft (TBinary ShiftLeft), binary ShiftRight (TBinary ShiftRight) ]
        , [ comparison GTEQ (TCompare GTEQ)
          , comparison GreaterThan (TCompare GreaterThan)
          , comparison Magnitude (TCompare Magnitude)
          , comparison LTEQ (TCompare LTEQ)
          , comparison LessThan (TCompare LessThan) ]
        , [ comparison Equal (TCompare Equal)
          , comparison Different (TCompare Different)
          , bin (Binary BitAnd) (TBinary BitAnd) AssocNone
          , bin (Binary BitOr) (TBinary BitOr) AssocNone
          , bin (Binary BitXor) (TBinary BitXor) AssocNone
          , comparison Matches (TCompare Matches)
          , comparison DoesNotMatch (TCompare DoesNotMatch) ]
        , [ bin And TAnd AssocLeft ]
        , [ bin Or TOr AssocLeft ]
        , [ bin Assignment Assign AssocRight ]
        , [ Infix (Augmented <$> tok augmented) AssocLeft ]
        , [ bin Sequence Newline AssocLeft ]
        ]
 
expr :: P Expr
expr = (exact Newline *> expr) <|> buildExpressionParser table term <|> term

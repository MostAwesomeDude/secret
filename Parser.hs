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

literal :: P Literal
literal = tok go <?> "literal"
    where
    go (TChar c) = Just $ EChar c
    go (TInt i) = Just $ EInteger i
    go (TFloat f) = Just $ EFloat f
    go (TURI u) = Just $ EURI u
    go _ = Nothing

-- noun :: (Monad m, TokenParsing m) => m Noun
-- noun = Noun <$> ident eStyle
--    <|> QLHole <$> (char '$' *> braces natural)
--    <|> QPHole <$> (char '@' *> braces natural)
-- 
-- bops :: [(BOp, String)]
-- bops = zip (enumFrom Add) l
--     where
--     l = ["+", "&", "|", "^", "/", "//", "%%", "*", "**", "%", "<<", ">>", "-"]
-- 
-- augOp :: (Monad m, TokenParsing m) => m BOp
-- augOp = choice $ map (\(op, s) -> reserve eOp (s ++ "=") *> pure op) bops
-- 
-- interval :: (Monad m, TokenParsing m) => m Interval
-- interval = reserve eOp ".." *> pure Through
--        <|> reserve eOp "..!" *> pure Till
-- 
-- exit :: (Monad m, TokenParsing m) => m Exit
-- exit = choice
--     [ reserve eStyle "break" *> pure Break
--     , reserve eStyle "continue" *> pure Continue
--     , reserve eStyle "return" *> pure Return
--     ]
-- 
-- pListAnd :: (Monad m, TokenParsing m) => m Pattern
-- pListAnd = do
--     ps <- brackets (sepBy pattern comma)
--     reserve eStyle "+"
--     p <- pattern
--     return $ PListAnd ps p
-- 
-- namer :: (Monad m, TokenParsing m) => m Pattern
-- namer = Namer <$> noun <* reserve eStyle ":" <*> expr
-- 
-- pattern :: (Monad m, TokenParsing m) => m Pattern
-- pattern = choice
--     [ Varying <$> (reserve eStyle "var" *> pattern)
--     , try $ pListAnd
--     , PList <$> brackets (sepBy pattern comma)
--     , ExactMatch <$> (reserve eOp "==" *> expr)
--     , try namer
--     , Final <$> noun ]
--     <?> "pattern"
-- 
-- quasi :: (Monad m, TokenParsing m) => m String
-- quasi = highlight StringLiteral $ do
--     void $ char '`'
--     manyTill anyChar (char '`')
--     <?> "quasiquote"
-- 
-- mapPair :: (Monad m, TokenParsing m) => m (Expr, Expr)
-- mapPair = do
--     k <- expr
--     reserve eOp "=>"
--     v <- expr
--     return (k, v)
--     <?> "pair"
-- 
-- ifExpr :: (Monad m, TokenParsing m) => m Expr
-- ifExpr = do
--     reserve eStyle "if"
--     cond <- parens expr
--     t <- braces expr
--     mf <- optional $ do
--         reserve eStyle "else"
--         braces expr <|> ifExpr
--     return $ case mf of
--         Just f  -> If cond t f
--         Nothing -> If cond t $ LitExpr Null
-- 
-- matchExpr :: (Monad m, TokenParsing m) => m (Pattern, Expr)
-- matchExpr = do
--     reserve eStyle "match"
--     p <- pattern
--     clause <- braces expr
--     return (p, clause)
-- 
-- switchExpr :: (Monad m, TokenParsing m) => m Expr
-- switchExpr = do
--     reserve eStyle "switch"
--     switch <- parens expr
--     cases <- many matchExpr
--     return $ Switch switch cases
-- 
-- tryExpr :: (Monad m, TokenParsing m) => m Expr
-- tryExpr = do
--     reserve eStyle "try"
--     action <- braces expr
--     catches <- many $ do
--         reserve eStyle "catch"
--         p <- pattern
--         clause <- braces expr
--         return (p, clause)
--     mfinally <- optional $ reserve eStyle "finally" *> braces expr
--     return $ case mfinally of
--         Nothing      -> Try action catches
--         Just finally -> TryFinally action catches finally
-- 
-- forExpr :: (Monad m, TokenParsing m) => m Expr
-- forExpr = do
--     reserve eStyle "for"
--     (kpattern, vpattern) <- mapPair
--     reserve eStyle "in"
--     cexpr <- expr
--     bexpr <- braces expr
--     return $ For kpattern vpattern cexpr bexpr
-- 
-- exitExpr :: (Monad m, TokenParsing m) => m Expr
-- exitExpr = EjectExit <$> exit <*> expr <?> "exit"
-- 
-- methodExpr :: (Monad m, TokenParsing m) => m Expr
-- methodExpr = do
--     reserve eStyle "to"
--     name <- noun
--     ps <- parens (sepBy pattern comma) <|> pure []
--     -- Guard on the return value.
--     rv <- reserve eOp ":" *> expr
--     body <- expr
--     return $ Function name ps rv body
-- 
-- defineExpr :: (Monad m, TokenParsing m) => m Expr
-- defineExpr = do
--     reserve eStyle "def"
--     try define <|> funcobj
--     where
--     define = do
--         binding <- pattern
--         reserve eOp ":="
--         body <- expr
--         return $ Define binding body
--     funcobj = do
--         n <- noun
--         object n <|> function n
--     function name = do
--         ps <- parens $ sepBy pattern comma
--         -- Guard on the return value.
--         rv <- reserve eOp ":" *> expr
--         body <- expr
--         return $ Function name ps rv body
--     object name = braces $ do
--         methods <- many $ methodExpr <|> (reserve eOp "\n" *> pure (LitExpr Null))
--         match <- optional matchExpr
--         return $ Object name methods match
-- 
-- term :: (Monad m, TokenParsing m) => m Expr
-- term = choice
--     -- Yes, it's possible to incorrectly pick literals: "0..end"
--     [ try $ LitExpr <$> literal
--     , parens expr
--     , Scope <$> braces expr
--     , try $ EMap <$> brackets (sepBy mapPair comma)
--     , EList <$> brackets (sepBy expr comma)
--     , ifExpr
--     , switchExpr
--     , tryExpr
--     , forExpr
--     , exitExpr
--     , Escape <$> (reserve eStyle "escape" *> noun) <*> braces expr
--     , While <$> (reserve eStyle "while" *> parens expr) <*> braces expr
--     , defineExpr
--     , Quasi "simple" <$> token quasi
--     , try $ Quasi <$> ident eStyle <*> token quasi
--     , NounExpr <$> noun
--     -- Swallow newlines not consumed by anybody else.
--     , token (many (char '\n')) *> pure (LitExpr Null) ]
--     <?> "primitive expression"
-- 
-- bin :: (Monad m, TokenParsing m)
--     => (a -> a -> a) -> String -> Assoc -> Operator m a
-- bin cons sym = Infix (reserve eOp sym *> pure cons)
-- 
-- binary :: (Monad m, TokenParsing m) => BOp -> String -> Operator m Expr
-- binary op sym = bin (Binary op) sym AssocLeft
-- 
-- comparison :: (Monad m, TokenParsing m) => COp -> String -> Operator m Expr
-- comparison op sym = bin (Comparison op) sym AssocNone
-- 
-- pre :: (Monad m, TokenParsing m) => String -> UOp -> Operator m Expr
-- pre s op = Prefix (reserve eOp s *> pure (Unary op))
-- 
-- table :: (Monad m, TokenParsing m) => OperatorTable m Expr
-- table = [ [ Postfix (flip Arguments <$> parens (sepBy expr comma)) ]
--         , [ bin Call "." AssocLeft ]
--         , [ bin Send "<-" AssocLeft
--           , bin Property "::" AssocLeft
--           , Postfix (flip Index <$> brackets (sepBy expr comma)) ]
--         , [ pre "!" Not, pre "~" Complement, pre "-" Negate ]
--         , [ binary Power "**" ]
--         , [ binary Multiply "*"
--           , binary FloorDivide "//"
--           , binary Divide "/"
--           , binary Modulus "%%"
--           , binary Remainder "%" ]
--         , [ binary Add "+", binary Subtract "-" ]
--         , [ binary ShiftLeft "<<", binary ShiftRight ">>" ]
--         , [ comparison GTEQ ">="
--           , comparison GreaterThan ">"
--           , comparison Magnitude "<=>"
--           , comparison LTEQ "<="
--           , comparison LessThan "<" ]
--         , [ comparison Equal "=="
--           , comparison Different "!="
--           , bin (Binary BitAnd) "&" AssocNone
--           , bin (Binary BitOr) "|" AssocNone
--           , bin (Binary BitXor) "^" AssocNone
--           , comparison Match "=~"
--           , comparison NoMatch "!~" ]
--         , [ bin And "&&" AssocLeft ]
--         , [ bin Or "||" AssocLeft ]
--         , [ bin Assign ":=" AssocRight ]
--         , [ Infix (Augmented <$> augOp) AssocLeft ]
--         , [ bin Sequence ";" AssocLeft ]
--         , [ bin Sequence "\n" AssocLeft ]
--         ]
--  
-- expr :: (Monad m, TokenParsing m) => m Expr
-- expr = (symbol "\n" *> expr) <|> buildExpressionParser table term <|> term

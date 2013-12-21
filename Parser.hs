module Parser where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Trifecta.Parser

import Expression

literal :: TokenParsing m => m Literal
literal = choice [ EURI <$> (char '<' *> manyTill anyChar (char '>'))
                 , EChar <$> charLiteral
                 , EString <$> stringLiteral
                 , either EInteger EFloat <$> naturalOrDouble
                 , symbol "null" *> pure Null ]

noun :: TokenParsing m => m Noun
noun = highlight Identifier $ Noun <$> token (some (oneOf cs))
   <|> QLHole <$> (char '$' *> braces natural)
   <|> QPHole <$> (char '@' *> braces natural)
   where
    cs = "_" ++ ['a'..'z'] ++ ['A'..'Z']

bops :: [(BOp, String)]
bops = zip (enumFrom Add) l
    where
    l = ["+", "&", "|", "^", "/", "//", "%%", "*", "**", "%", "<<", ">>", "-"]

bop :: TokenParsing m => m BOp
bop = choice $ map (\(op, s) -> symbol s *> pure op) bops

augOp :: TokenParsing m => m BOp
augOp = choice $ map (\(op, s) -> symbol (s ++ "=") *> pure op) bops

interval :: TokenParsing m => m Interval
interval = symbol ".." *> pure Through <|> symbol "..!" *> pure Till

exit :: TokenParsing m => m Exit
exit = choice
    [ symbol "break" *> pure Break
    , symbol "continue" *> pure Continue
    , symbol "return" *> pure Return
    ]

pListAnd :: (Monad m, TokenParsing m) => m Pattern
pListAnd = PListAnd <$> brackets (sepBy pattern comma) <* symbol "+" <*> pattern

namer :: (Monad m, TokenParsing m) => m Pattern
namer = choice
    [ try $ Namer <$> noun <* symbol ":" <*> expr
    , flip Namer (NounExpr (Noun "settable")) <$> noun
    ]

pattern :: (Monad m, TokenParsing m) => m Pattern
pattern = choice
    [ try $ pListAnd
    , PList <$> brackets (sepBy pattern comma)
    , ExactMatch <$> (symbol "==" *> expr)
    , namer
    , Final <$> noun
    ]

quasi :: (Monad m, TokenParsing m) => m String
quasi = highlight StringLiteral $ do
    void $ char '`'
    manyTill anyChar (char '`')
    <?> "quasiquote"

mapPair :: (Monad m, TokenParsing m) => m (Expr, Expr)
mapPair = do
    k <- expr
    void $ symbol "=>"
    v <- expr
    return (k, v)
    <?> "pair"

ifExpr :: (Monad m, TokenParsing m) => m Expr
ifExpr = do
    void $ symbol "if"
    cond <- parens expr
    t <- braces expr
    mf <- optional $ do
        void $ symbol "else"
        braces expr <|> ifExpr
    return $ case mf of
        Just f  -> If cond t f
        Nothing -> If cond t $ LitExpr Null

switchExpr :: (Monad m, TokenParsing m) => m Expr
switchExpr = do
    void $ symbol "switch"
    switch <- parens expr
    cases <- many $ do
        void $ symbol "match"
        p <- pattern
        clause <- braces expr
        return (p, clause)
    return $ Switch switch cases

tryExpr :: (Monad m, TokenParsing m) => m Expr
tryExpr = do
    void $ symbol "try"
    action <- braces expr
    catches <- many $ do
        void $ symbol "catch"
        p <- pattern
        clause <- braces expr
        return (p, clause)
    mfinally <- optional $ symbol "finally" *> braces expr
    return $ case mfinally of
        Nothing      -> Try action catches
        Just finally -> TryFinally action catches finally

forExpr :: (Monad m, TokenParsing m) => m Expr
forExpr = do
    void $ symbol "for"
    (kpattern, vpattern) <- mapPair
    void $ symbol "in"
    cexpr <- expr
    bexpr <- braces expr
    return $ For kpattern vpattern cexpr bexpr

identifier :: (Monad m, TokenParsing m) => m String
identifier = highlight Identifier (some letter) <?> "identifier"

defineExpr :: (Monad m, TokenParsing m) => m Expr
defineExpr = do
    void $ symbol "def"
    binding <- pattern
    void $ symbol ":="
    body <- expr
    return $ Define binding body

functionExpr :: (Monad m, TokenParsing m) => m Expr
functionExpr = do
    void $ symbol "def"
    name <- noun
    ps <- parens (sepBy pattern comma) <|> pure []
    -- Guard on the return value.
    rv <- symbol ":" *> expr
    body <- expr
    return $ Function name ps rv body

exitExpr :: (Monad m, TokenParsing m) => m Expr
exitExpr = EjectExit <$> exit <*> expr

term :: (Monad m, TokenParsing m) => m Expr
term = choice
    [ LitExpr <$> literal
    , parens expr
    , Scope <$> braces expr
    , try $ EMap <$> brackets (sepBy mapPair comma)
    , EList <$> brackets (sepBy expr comma)
    , ifExpr
    , switchExpr
    , tryExpr
    , forExpr
    , exitExpr
    , Escape <$> (symbol "escape" *> noun) <*> braces expr
    , While <$> (symbol "while" *> parens expr) <*> braces expr
    , try functionExpr
    , defineExpr
    , Quasi "simple" <$> token quasi
    , try $ Quasi <$> identifier <*> token quasi
    , NounExpr <$> noun ]
    <?> "primitive expression"

bin :: TokenParsing m => (a -> a -> a) -> String -> Assoc -> Operator m a
bin cons sym = Infix (pure cons <* symbol sym)

binary :: TokenParsing m => BOp -> String -> Operator m Expr
binary op sym = bin (Binary op) sym AssocLeft

comparison :: TokenParsing m => COp -> String -> Operator m Expr
comparison op sym = bin (Comparison op) sym AssocNone

pre :: TokenParsing m => String -> UOp -> Operator m Expr
pre s op = Prefix (symbol s *> pure (Unary op))

table :: (Monad m, TokenParsing m) => OperatorTable m Expr
table = [ [ Postfix (flip Arguments <$> parens (sepBy expr comma)) ]
        , [ bin Call "." AssocLeft ]
        , [ bin Send "<-" AssocLeft
          , bin Property "::" AssocLeft
          , Postfix (flip Index <$> brackets (sepBy expr comma)) ]
        , [ pre "!" Not, pre "~" Complement, pre "-" Negate ]
        , [ binary Power "**" ]
        , [ binary Multiply "*"
          , binary FloorDivide "//"
          , binary Divide "/"
          , binary Modulus "%%"
          , binary Remainder "%" ]
        , [ binary Add "+", binary Subtract "-" ]
        , [ binary ShiftLeft "<<", binary ShiftRight ">>" ]
        , [ comparison GTEQ ">="
          , comparison GreaterThan ">"
          , comparison Magnitude "<=>"
          , comparison LTEQ "<="
          , comparison LessThan "<" ]
        , [ comparison Equal "=="
          , comparison Different "!="
          , bin (Binary BitAnd) "&" AssocNone
          , bin (Binary BitOr) "|" AssocNone
          , bin (Binary BitXor) "^" AssocNone
          , comparison Match "=~"
          , comparison NoMatch "!~" ]
        , [ bin And "&&" AssocLeft ]
        , [ bin Or "||" AssocLeft ]
        , [ bin Assign ":=" AssocRight ]
        , [ Infix (Augmented <$> augOp) AssocLeft ]
        , [ bin Sequence ";" AssocLeft ]
        ]
 
expr :: (Monad m, TokenParsing m) => m Expr
expr = buildExpressionParser table term <|> term

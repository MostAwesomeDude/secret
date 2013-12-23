module Parser where

import Control.Applicative
import Control.Monad
import qualified Data.HashSet as HS
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Highlight

import Expression

eStyle :: (Monad m, TokenParsing m) => IdentifierStyle m
eStyle = IdentifierStyle
    { _styleName = "E identifier"
    , _styleStart = oneOf $ "_" ++ ['a'..'z'] ++ ['A'..'Z']
    , _styleLetter = oneOf $ "_" ++ ['a'..'z'] ++ ['A'..'Z']
    , _styleReserved = HS.fromList [ "break", "catch", "continue", "def"
                                   , "else", "escape", "finally", "for", "if"
                                   , "in", "match", "null", "return"
                                   , "switch", "to", "try", "while" ]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

literal :: (Monad m, TokenParsing m) => m Literal
literal = choice
    [ EURI <$> (char '<' *> manyTill anyChar (char '>'))
    , EChar <$> charLiteral
    , EString <$> stringLiteral
    , either EInteger EFloat <$> naturalOrDouble
    , reserve eStyle "null" *> pure Null ]
    <?> "literal"

noun :: (Monad m, TokenParsing m) => m Noun
noun = Noun <$> ident eStyle
   <|> QLHole <$> (char '$' *> braces natural)
   <|> QPHole <$> (char '@' *> braces natural)

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

exit :: (Monad m, TokenParsing m) => m Exit
exit = choice
    [ reserve eStyle "break" *> pure Break
    , reserve eStyle "continue" *> pure Continue
    , reserve eStyle "return" *> pure Return
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
    , Final <$> noun ]
    <?> "pattern"

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
    reserve eStyle "if"
    cond <- parens expr
    t <- braces expr
    mf <- optional $ do
        reserve eStyle "else"
        braces expr <|> ifExpr
    return $ case mf of
        Just f  -> If cond t f
        Nothing -> If cond t $ LitExpr Null

matchExpr :: (Monad m, TokenParsing m) => m (Pattern, Expr)
matchExpr = do
    reserve eStyle "match"
    p <- pattern
    clause <- braces expr
    return (p, clause)

switchExpr :: (Monad m, TokenParsing m) => m Expr
switchExpr = do
    reserve eStyle "switch"
    switch <- parens expr
    cases <- many matchExpr
    return $ Switch switch cases

tryExpr :: (Monad m, TokenParsing m) => m Expr
tryExpr = do
    reserve eStyle "try"
    action <- braces expr
    catches <- many $ do
        reserve eStyle "catch"
        p <- pattern
        clause <- braces expr
        return (p, clause)
    mfinally <- optional $ reserve eStyle "finally" *> braces expr
    return $ case mfinally of
        Nothing      -> Try action catches
        Just finally -> TryFinally action catches finally

forExpr :: (Monad m, TokenParsing m) => m Expr
forExpr = do
    reserve eStyle "for"
    (kpattern, vpattern) <- mapPair
    reserve eStyle "in"
    cexpr <- expr
    bexpr <- braces expr
    return $ For kpattern vpattern cexpr bexpr

exitExpr :: (Monad m, TokenParsing m) => m Expr
exitExpr = EjectExit <$> exit <*> expr <?> "exit"

methodExpr :: (Monad m, TokenParsing m) => m Expr
methodExpr = do
    reserve eStyle "to"
    name <- noun
    ps <- parens (sepBy pattern comma) <|> pure []
    -- Guard on the return value.
    rv <- symbol ":" *> expr
    body <- expr
    return $ Function name ps rv body

defineExpr :: (Monad m, TokenParsing m) => m Expr
defineExpr = do
    reserve eStyle "def"
    try define <|> funcobj
    where
    define = do
        binding <- pattern
        void $ symbol ":="
        body <- expr
        return $ Define binding body
    funcobj = do
        n <- noun
        function n <|> object n
    function name = do
        ps <- parens $ sepBy pattern comma
        -- Guard on the return value.
        rv <- symbol ":" *> expr
        body <- expr
        return $ Function name ps rv body
    object name = do
        braces $ do
            methods <- many methodExpr
            match <- optional matchExpr
            return $ Object name methods match

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
    , Escape <$> (reserve eStyle "escape" *> noun) <*> braces expr
    , While <$> (reserve eStyle "while" *> parens expr) <*> braces expr
    , defineExpr
    , Quasi "simple" <$> token quasi
    , try $ Quasi <$> ident eStyle <*> token quasi
    , NounExpr <$> noun
    -- Swallow newlines not consumed by anybody else.
    , token (many (char '\n')) *> pure (LitExpr Null) ]
    <?> "primitive expression"

bin :: TokenParsing m => (a -> a -> a) -> String -> Assoc -> Operator m a
bin cons sym = Infix (symbol sym *> pure cons)

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
        , [ bin Sequence "\n" AssocLeft ]
        ]
 
expr :: (Monad m, TokenParsing m) => m Expr
expr = (symbol "\n" *> expr) <|> buildExpressionParser table term <|> term

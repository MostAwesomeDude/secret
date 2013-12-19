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

data Literal = Null
             | EInteger Integer
             | EFloat Double
             | EChar Char
             | EString String
             | EURI String
    deriving (Eq, Show)

literal :: TokenParsing m => m Literal
literal = choice [ EURI <$> (char '<' *> manyTill anyChar (char '>'))
                 , EChar <$> charLiteral
                 , EString <$> stringLiteral
                 , either EInteger EFloat <$> naturalOrDouble
                 , symbol "null" *> pure Null ]

formatLiteral :: Literal -> String
formatLiteral Null = "null"
formatLiteral (EInteger i) = show i
formatLiteral (EFloat f) = show f
formatLiteral (EChar c) = show c
formatLiteral (EString s) = show s
formatLiteral (EURI u) = '<' : u ++ ">"

data Noun = Noun String
          | QLHole Integer
          | QPHole Integer
    deriving (Eq, Show)

noun :: TokenParsing m => m Noun
noun = highlight Identifier $ Noun <$> token (some (oneOf cs))
   <|> QLHole <$> (char '$' *> braces natural)
   <|> QPHole <$> (char '@' *> braces natural)
   where cs = "_" ++ ['a'..'z'] ++ ['A'..'Z']

formatNoun :: Noun -> String
formatNoun (Noun s) = s
formatNoun (QLHole i) = "${" ++ show i ++ "}"
formatNoun (QPHole i) = "@{" ++ show i ++ "}"

data UOp = Complement | Negate | Not
    deriving (Enum, Eq, Ord, Show)

formatUOp :: UOp -> String
formatUOp Complement = "~"
formatUOp Negate = "-"
formatUOp Not = "!"

data BOp = Add | BinaryAnd | BinaryOr | BinaryXor | Divide | FloorDivide
         | Modulus | Multiply | Power | Remainder | ShiftLeft | ShiftRight
         | Subtract
    deriving (Enum, Eq, Ord, Show)

bops :: [(BOp, String)]
bops = zip (enumFrom Add) l
    where
    l = ["+", "&", "|", "^", "/", "//", "%%", "*", "**", "%", "<<", ">>", "-"]

bop :: TokenParsing m => m BOp
bop = choice $ map (\(op, s) -> symbol s *> pure op) bops

augOp :: TokenParsing m => m BOp
augOp = choice $ map (\(op, s) -> symbol (s ++ "=") *> pure op) bops

formatBOp :: BOp -> String
formatBOp op = maybe "???" id (lookup op bops)

data Interval = Through | Till
    deriving (Enum, Eq, Ord, Show)

interval :: TokenParsing m => m Interval
interval = symbol ".." *> pure Through <|> symbol "..!" *> pure Till

formatInterval :: Interval -> String
formatInterval Through = ".."
formatInterval Till = "..!"

data Expr = LitExpr Literal
          | NounExpr Noun
          | Unary UOp Expr
          | Binary BOp Expr Expr
          | Range Interval Expr Expr
          | Equals Expr Expr
          | Quasi String String
          | EList [Expr]
          | EMap [(Expr, Expr)]
          | Scope Expr
          | Sequence Expr Expr
          | Augmented BOp Expr Expr
          | Assign Expr Expr
          | If Expr Expr Expr
          | Switch Expr [(Expr, Expr)]
          | Try Expr [(Expr, Expr)]
          | TryFinally Expr [(Expr, Expr)] Expr
          | Escape Expr Expr
          | While Expr Expr
          | For Expr Expr Expr Expr
          | Arguments Expr [Expr]
          | Index Expr [Expr]
          | Property Expr Expr
          | Call Expr Expr
          | Send Expr Expr
    deriving (Show)

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
        k <- expr
        v <- braces expr
        return (k, v)
    return $ Switch switch cases

tryExpr :: (Monad m, TokenParsing m) => m Expr
tryExpr = do
    void $ symbol "try"
    action <- braces expr
    catches <- many $ do
        void $ symbol "catch"
        pattern <- expr
        clause <- braces expr
        return (pattern, clause)
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
    , Escape <$> (symbol "escape" *> expr) <*> braces expr
    , While <$> (symbol "while" *> parens expr) <*> braces expr
    , Quasi "simple" <$> token quasi
    , try $ Quasi <$> identifier <*> token quasi
    , NounExpr <$> noun ]
    <?> "primitive expression"

bin :: TokenParsing m => (a -> a -> a) -> String -> Assoc -> Operator m a
bin cons sym = Infix (pure cons <* symbol sym)

binary :: TokenParsing m => BOp -> String -> Operator m Expr
binary op sym = bin (Binary op) sym AssocLeft

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
        , [ bin Equals "==" AssocNone ]
        , [ binary BinaryAnd "&&" ]
        , [ binary BinaryOr "||" ]
        , [ bin Assign ":=" AssocRight ]
        , [ Infix (Augmented <$> augOp) AssocLeft ]
        , [ bin Sequence ";" AssocLeft ]
        ]
 
expr :: (Monad m, TokenParsing m) => m Expr
expr = buildExpressionParser table term

formatPair :: (Expr, Expr) -> String
formatPair (f, s) = formatExpr f ++ "=>" ++ formatExpr s

formatMatch :: (Expr, Expr) -> String
formatMatch (p, e) = "match " ++ formatExpr p ++ "{" ++ formatExpr e ++ "}"

formatCatch :: (Expr, Expr) -> String
formatCatch (p, e) = "catch (" ++ formatExpr p ++ "{" ++ formatExpr e ++ "}"

formatExpr :: Expr -> String
formatExpr (LitExpr l) = formatLiteral l
formatExpr (NounExpr n) = formatNoun n
formatExpr (Unary op e) = formatUOp op ++ formatExpr e
formatExpr (Binary op e e') = formatExpr e ++ formatBOp op ++ formatExpr e'
formatExpr (Range i e e') = formatExpr e ++ formatInterval i ++ formatExpr e'
formatExpr (Equals e e') = formatExpr e ++ "==" ++ formatExpr e'
formatExpr (Quasi s q) = s ++ "`" ++ q ++ "`"
formatExpr (EList es) = "[" ++ intercalate "," (map formatExpr es) ++ "]"
formatExpr (EMap ts) = "[" ++ intercalate "," (map formatPair ts) ++ "]"
formatExpr (Scope e) = "{" ++ formatExpr e ++ "}"
formatExpr (Sequence e e') = formatExpr e ++ "\n" ++ formatExpr e'
formatExpr (Augmented op e e') = formatExpr e ++ formatBOp op ++ "=" ++ formatExpr e'
formatExpr (Assign e e') = formatExpr e ++ ":=" ++ formatExpr e'
formatExpr (If c t e) = "if (" ++ formatExpr c ++ "){" ++ formatExpr t ++ "} else {" ++ formatExpr e ++ "}"
formatExpr (Switch c ts) = "switch (" ++ formatExpr c ++ ") {" ++ concatMap formatMatch ts ++ "}"
formatExpr (Try b cs) = "try {" ++ formatExpr b ++ "}" ++ concatMap formatCatch cs
formatExpr (TryFinally b cs f) = "try {" ++ formatExpr b ++ "}" ++ concatMap formatCatch cs ++ "finally {" ++ formatExpr f ++ "}"
formatExpr (Escape e b) = "escape " ++ formatExpr e ++ " {" ++ formatExpr b ++ "}"
formatExpr (While c b) = "while (" ++ formatExpr c ++ ") {" ++ formatExpr b ++ "}"
formatExpr (For k v c b) = "for " ++ formatPair (k, v) ++ " in " ++ formatExpr c ++ " {" ++ formatExpr b ++ "}"
formatExpr (Arguments e es) = formatExpr e ++ "(" ++ intercalate "," (map formatExpr es) ++ ")"
formatExpr (Index e es) = formatExpr e ++ "[" ++ intercalate "," (map formatExpr es) ++ "]"
formatExpr (Property e p) = formatExpr e ++ "::" ++ formatExpr p
formatExpr (Call e m) = formatExpr e ++ "." ++ formatExpr m
formatExpr (Send e m) = formatExpr e ++ "<-" ++ formatExpr m

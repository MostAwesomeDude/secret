-- Copyright (C) 2014 Google Inc. All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License. You may obtain a copy
-- of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
module Printer where

import Text.PrettyPrint.ANSI.Leijen

import Expression

instance Pretty Literal where
    pretty Null = string "null"
    pretty (EInteger i) = string $ show i
    pretty (EFloat f) = string $ show f
    pretty (EChar c) = string $ show c
    pretty (EString s) = string $ show s
    pretty (EURI u) = string $ '<' : u ++ ">"

instance Pretty Noun where
    pretty (Noun s) = string s
    pretty (QLHole i) = string $ "${" ++ show i ++ "}"
    pretty (QPHole i) = string $ "@{" ++ show i ++ "}"

instance Pretty UOp where
    pretty Complement = char '~'
    pretty Negate = char '-'
    pretty Not = char '!'

instance Pretty BOp where
    pretty Add = char '+'
    pretty BitAnd = char '&'
    pretty BitOr = char '|'
    pretty BitXor = char '^'
    pretty Divide = char '/'
    pretty FloorDivide = string "//"
    pretty Modulus = string "%%"
    pretty Multiply = char '*'
    pretty Power = string "**"
    pretty Remainder = char '%'
    pretty ShiftLeft = string "<<"
    pretty ShiftRight = string ">>"
    pretty Subtract = char '-'

instance Pretty COp where
    pretty Different = string "!="
    pretty Equal = string "=="
    pretty GTEQ = string ">="
    pretty GreaterThan = char '>'
    pretty LTEQ = string "<="
    pretty LessThan = char '<'
    pretty Magnitude = string "<=>"
    pretty Matches = string "=~"
    pretty DoesNotMatch = string "!~"

instance Pretty Interval where
    pretty Through = string ".."
    pretty Till = string "..!"

instance Pretty Exit where
    pretty Break = string "break"
    pretty Continue = string "continue"
    pretty Return = string "return"

instance Pretty Pattern where
    pretty (SuchThat p e) = pretty p <+> char '?' <+> pretty e
    pretty (PList ps) = formatList ps
    pretty (PListAnd ps p) = formatList ps <+> char '+' <+> pretty p
    pretty (ExactMatch e) = string "==" <> pretty e
    pretty (Namer n e) = pretty n <+> char ':' <> pretty e
    pretty (Varying p) = string "var" <+> pretty p
    pretty (Final n) = pretty n

formatPair :: (Pretty a, Pretty b) => (a, b) -> Doc
formatPair (f, s) = pretty f </> string "=>" </> pretty s

formatMatch :: (Pattern, Expr) -> Doc
formatMatch (p, e) = string "match" </> pretty p </> brace (pretty e)

formatCatch :: (Pattern, Expr) -> Doc
formatCatch (p, e) = string "catch" </> parens (pretty p) </> brace (pretty e)

formatList :: Pretty a => [a] -> Doc
formatList xs = brackets . align . cat $ punctuate comma $ map pretty xs

paren :: Pretty a => [a] -> Doc
paren xs = parens . align . cat $ punctuate comma $ map pretty xs

brace :: Doc -> Doc
brace e = char '{' <$> nest 2 e <$> char '}'

instance Pretty Expr where
    pretty (LitExpr l) = pretty l
    pretty (NounExpr n) = pretty n
    pretty (Unary op e) = pretty op <> pretty e
    pretty (Binary op e e') = pretty e <+> pretty op <+> pretty e'
    pretty (Comparison op e e') = pretty e <+> pretty op <+> pretty e'
    pretty (Range i e e') = pretty e <//> pretty i <//> pretty e'
    pretty (Or e e') = pretty e <+> string "||" <+> pretty e'
    pretty (And e e') = pretty e <+> string "&&" <+> pretty e'
    pretty (Quasi s q) = string s <> char '`' <> string q <> char '`'
    pretty (EList es) = brackets . align . cat $ punctuate comma $ map pretty es
    pretty (EMap ts) =  brackets . align . cat $ punctuate comma $ map formatPair ts
    pretty (Scope e) = brace $ pretty e
    pretty (Sequence e e') = pretty e <$> pretty e'
    pretty (Augmented op e e') = pretty e <+> pretty op <> char '=' <+> pretty e'
    pretty (Assignment e e') = pretty e <+> string ":=" <+> pretty e'
    pretty (Define p e) = string "def" <+> pretty p <+> string ":=" <+> pretty e
    pretty (Function n ps rv e) = string "def" <+> pretty n <+> paren ps <+> char ':' <> pretty rv <+> pretty e
    pretty (Method n ps rv e) = string "to" <+> pretty n <+> paren ps <+> char ':' <> pretty rv <+> pretty e
    pretty (If c t e) = string "if" <+> parens (pretty c) <+> brace (pretty t) <+> string "else" <+> brace (pretty e)
    pretty (Switch c ts) = string "switch" <+> parens (pretty c) <+> (cat (map formatMatch ts))
    pretty (Try b cs) = string "try" <+> brace (pretty b) <+> brace (cat (map formatCatch cs))
    pretty (TryFinally b cs f) = string "try" <+> brace (pretty b) <+> brace (cat (map formatCatch cs)) <+> string "finally" <+> brace (pretty f)
    pretty (Escape e b) = string "escape" <+> pretty e <+> brace (pretty b)
    pretty (While c b) = string "while" <+> parens (pretty c) <+> brace (pretty b)
    pretty (For k v c b) = string "for" <+> formatPair (k, v) <+> string "in" <+> pretty c <+> brace (pretty b)
    pretty (Arguments e es) = pretty e <> paren es
    pretty (Index e es) = pretty e <+> formatList es
    pretty (EProperty e p) = pretty e <> string "::" <> pretty p
    pretty (ECall e m) = pretty e <> char '.' <> pretty m
    pretty (ESend e m) = pretty e <> string "<-" <> pretty m
    pretty (EjectExit e e') = pretty e <+> pretty e'
    pretty (Object n ms mm) = string "def" <+> pretty n <+> brace (cat (map pretty ms) <$> maybe empty pretty mm)

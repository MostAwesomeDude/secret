{
module Lexer where

import Expression hiding (Expr(..), Exit(..))
}

%wrapper "basic"

tokens :-

    [\ ]+           ;
    \;\n+|\;|\n+    { \_ -> Newline }
    :               { \_ -> Colon }
    \.              { \_ -> Dot }
    \,              { \_ -> Comma }
    \{              { \_ -> OpenBrace }
    \}              { \_ -> CloseBrace }
    \[              { \_ -> OpenBracket }
    \]              { \_ -> CloseBracket }
    \(              { \_ -> OpenParen }
    \)              { \_ -> CloseParen }
    '.'             { \[_, c, _] -> TChar c }
    [0-9]+          { \s -> TInt $ read s }
    \"[^\"]*\"      { \s -> TString (init (tail s)) }
    \<[^\>]*\>      { \s -> TURI (init (tail s)) }
    [a-zA-Z]+       { TIdentifier }
    break           { \_ -> Break }
    catch           { \_ -> Catch }
    continue        { \_ -> Continue }
    def             { \_ -> Def }
    else            { \_ -> Else }
    escape          { \_ -> Escape }
    finally         { \_ -> Finally }
    for             { \_ -> For }
    if              { \_ -> If }
    in              { \_ -> In }
    match           { \_ -> Match }
    return          { \_ -> Return }
    switch          { \_ -> Switch }
    to              { \_ -> To }
    try             { \_ -> Try }
    var             { \_ -> Var }
    while           { \_ -> While }
    !=              { \_ -> TCompare Different }
    !\~             { \_ -> TCompare DoesNotMatch }
    \%              { \_ -> TBinary Remainder }
    \%\%            { \_ -> TBinary Modulus }
    \%\%=           { \_ -> TAugmented Modulus }
    \%=             { \_ -> TAugmented Remainder }
    &               { \_ -> TBinary BitAnd }
    &&              { \_ -> TAnd }
    &=              { \_ -> TAugmented BitAnd }
    \*              { \_ -> TBinary Multiply }
    \*\*            { \_ -> TBinary Power }
    \*\*=           { \_ -> TAugmented Power }
    \*=             { \_ -> TAugmented Multiply }
    \+              { \_ -> TBinary Add }
    \+=             { \_ -> TAugmented Add }
    \-              { \_ -> TBinary Subtract }
    \-=             { \_ -> TAugmented Subtract }
    \/              { \_ -> TBinary Divide }
    \/\/            { \_ -> TBinary FloorDivide }
    \/\/=           { \_ -> TAugmented FloorDivide }
    \/=             { \_ -> TAugmented Divide }
    ::              { \_ -> Property }
    :=              { \_ -> Assign }
    \<              { \_ -> TCompare LessThan }
    \<\-            { \_ -> Send }
    \<\<            { \_ -> TBinary ShiftLeft }
    \<\<=           { \_ -> TAugmented ShiftLeft }
    \<=             { \_ -> TCompare LTEQ }
    \<=\>           { \_ -> TCompare Magnitude }
    ==              { \_ -> TCompare Equal }
    =\~             { \_ -> TCompare Matches }
    \>              { \_ -> TCompare GreaterThan }
    \>=             { \_ -> TCompare GTEQ }
    \>\>            { \_ -> TBinary ShiftRight }
    \>\>=           { \_ -> TAugmented ShiftRight }
    \^              { \_ -> TBinary BitXor }
    \^=             { \_ -> TAugmented BitXor }
    \|              { \_ -> TBinary BitOr }
    \|=             { \_ -> TAugmented BitOr }
    \|\|            { \_ -> TOr }

{
data Token = Newline
           -- Primitive symbols
           | Colon
           | Dot
           | Comma
           | OpenBrace
           | CloseBrace
           | OpenBracket
           | CloseBracket
           | OpenParen
           | CloseParen
           -- Compound non-operator symbols
           | Assign
           | Property
           | Send
           | TAnd
           | TOr
           -- Keywords
           | Break
           | Catch
           | Continue
           | Def
           | Else
           | Escape
           | Finally
           | For
           | If
           | In
           | Match
           | Return
           | Switch
           | To
           | Try
           | Var
           | While
           -- Primitive objects and wrappers
           | TChar Char
           | TFloat Double
           | TInt Integer
           | TString String
           | TURI String
           | TIdentifier String
           | TUnary UOp
           | TBinary BOp
           | TAugmented BOp
           | TCompare COp
    deriving (Eq, Show)
}

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
    ::              { \_ -> Property }
    :=              { \_ -> Assign }
    =\>             { \_ -> Pair }
    \<\-            { \_ -> Send }
    \|\|            { \_ -> TOr }
    '.'             { \[_, c, _] -> TChar c }
    [0-9]+          { \s -> TInt $ read s }
    \"[^\"]*\"      { \s -> TString (init (tail s)) }
    `[^`]*`         { \s -> TQuasi (init (tail s)) }
    \<[^\>]*\>      { \s -> TURI (init (tail s)) }
    [a-zA-Z]+       { TIdentifier }
    break           { \_ -> TBreak }
    catch           { \_ -> TCatch }
    continue        { \_ -> TContinue }
    def             { \_ -> TDef }
    else            { \_ -> TElse }
    escape          { \_ -> TEscape }
    finally         { \_ -> TFinally }
    for             { \_ -> TFor }
    if              { \_ -> TIf }
    in              { \_ -> TIn }
    match           { \_ -> TMatch }
    return          { \_ -> TReturn }
    switch          { \_ -> TSwitch }
    to              { \_ -> TTo }
    try             { \_ -> TTry }
    var             { \_ -> TVar }
    while           { \_ -> TWhile }
    !               { \_ -> TUnary Not }
    \~              { \_ -> TUnary Complement }
    \-              { \_ -> TUnary Negate }
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
    \<              { \_ -> TCompare LessThan }
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
    \.\.            { \_ -> TInterval Through }
    \.\.\!          { \_ -> TInterval Till }

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
           | Pair
           | Property
           | Send
           | TAnd
           | TOr
           -- Keywords
           | TBreak
           | TCatch
           | TContinue
           | TDef
           | TElse
           | TEscape
           | TFinally
           | TFor
           | TIf
           | TIn
           | TMatch
           | TReturn
           | TSwitch
           | TTo
           | TTry
           | TVar
           | TWhile
           -- Primitive objects and wrappers
           | TChar Char
           | TFloat Double
           | TInt Integer
           | TString String
           | TURI String
           | TQuasi String
           | TIdentifier String
           | TUnary UOp
           | TBinary BOp
           | TAugmented BOp
           | TCompare COp
           | TInterval Interval
    deriving (Eq, Show)
}

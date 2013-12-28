{
module Lexer where

import Expression hiding (Expr(..), Exit(..))
}

%wrapper "posn"

@nl = \n+

tokens :-

    [\ ]+           ;
    @nl             { wrapPos $ \_ -> Newline }
    \;+             { wrapPos $ \_ -> Semicolon }
    :               { wrapPos $ \_ -> Colon }
    \.              { wrapPos $ \_ -> Dot }
    \,              { wrapPos $ \_ -> Comma }
    \{ @nl?         { wrapPos $ \_ -> OpenBrace }
    \}              { wrapPos $ \_ -> CloseBrace }
    \[              { wrapPos $ \_ -> OpenBracket }
    \]              { wrapPos $ \_ -> CloseBracket }
    \(              { wrapPos $ \_ -> OpenParen }
    \)              { wrapPos $ \_ -> CloseParen }
    ::              { wrapPos $ \_ -> Property }
    :=              { wrapPos $ \_ -> Assign }
    =\>             { wrapPos $ \_ -> Pair }
    \<\-            { wrapPos $ \_ -> Send }
    \|\|            { wrapPos $ \_ -> TOr }
    '.'             { wrapPos $ \[_, c, _] -> TChar c }
    [0-9]+          { wrapPos $ \s -> TInt $ read s }
    \"[^\"]*\"      { wrapPos $ \s -> TString (init (tail s)) }
    `[^`]*`         { wrapPos $ \s -> TQuasi (init (tail s)) }
    \<[^\>]*\>      { wrapPos $ \s -> TURI (init (tail s)) }
    break           { wrapPos $ \_ -> TBreak }
    catch           { wrapPos $ \_ -> TCatch }
    continue        { wrapPos $ \_ -> TContinue }
    def             { wrapPos $ \_ -> TDef }
    else            { wrapPos $ \_ -> TElse }
    escape          { wrapPos $ \_ -> TEscape }
    finally         { wrapPos $ \_ -> TFinally }
    for             { wrapPos $ \_ -> TFor }
    if              { wrapPos $ \_ -> TIf }
    in              { wrapPos $ \_ -> TIn }
    match           { wrapPos $ \_ -> TMatch }
    return          { wrapPos $ \_ -> TReturn }
    switch          { wrapPos $ \_ -> TSwitch }
    to              { wrapPos $ \_ -> TTo }
    try             { wrapPos $ \_ -> TTry }
    var             { wrapPos $ \_ -> TVar }
    while           { wrapPos $ \_ -> TWhile }
    -- Identifiers must be attempted after all keywords.
    [a-zA-Z]+       { wrapPos $ TIdentifier }
    !               { wrapPos $ \_ -> TUnary Not }
    \~              { wrapPos $ \_ -> TUnary Complement }
    \-              { wrapPos $ \_ -> TUnary Negate }
    !=              { wrapPos $ \_ -> TCompare Different }
    !\~             { wrapPos $ \_ -> TCompare DoesNotMatch }
    \%              { wrapPos $ \_ -> TBinary Remainder }
    \%\%            { wrapPos $ \_ -> TBinary Modulus }
    \%\%=           { wrapPos $ \_ -> TAugmented Modulus }
    \%=             { wrapPos $ \_ -> TAugmented Remainder }
    &               { wrapPos $ \_ -> TBinary BitAnd }
    &&              { wrapPos $ \_ -> TAnd }
    &=              { wrapPos $ \_ -> TAugmented BitAnd }
    \*              { wrapPos $ \_ -> TBinary Multiply }
    \*\*            { wrapPos $ \_ -> TBinary Power }
    \*\*=           { wrapPos $ \_ -> TAugmented Power }
    \*=             { wrapPos $ \_ -> TAugmented Multiply }
    \+              { wrapPos $ \_ -> TBinary Add }
    \+=             { wrapPos $ \_ -> TAugmented Add }
    \-              { wrapPos $ \_ -> TBinary Subtract }
    \-=             { wrapPos $ \_ -> TAugmented Subtract }
    \/              { wrapPos $ \_ -> TBinary Divide }
    \/\/            { wrapPos $ \_ -> TBinary FloorDivide }
    \/\/=           { wrapPos $ \_ -> TAugmented FloorDivide }
    \/=             { wrapPos $ \_ -> TAugmented Divide }
    \<              { wrapPos $ \_ -> TCompare LessThan }
    \<\<            { wrapPos $ \_ -> TBinary ShiftLeft }
    \<\<=           { wrapPos $ \_ -> TAugmented ShiftLeft }
    \<=             { wrapPos $ \_ -> TCompare LTEQ }
    \<=\>           { wrapPos $ \_ -> TCompare Magnitude }
    ==              { wrapPos $ \_ -> TCompare Equal }
    =\~             { wrapPos $ \_ -> TCompare Matches }
    \>              { wrapPos $ \_ -> TCompare GreaterThan }
    \>=             { wrapPos $ \_ -> TCompare GTEQ }
    \>\>            { wrapPos $ \_ -> TBinary ShiftRight }
    \>\>=           { wrapPos $ \_ -> TAugmented ShiftRight }
    \^              { wrapPos $ \_ -> TBinary BitXor }
    \^=             { wrapPos $ \_ -> TAugmented BitXor }
    \|              { wrapPos $ \_ -> TBinary BitOr }
    \|=             { wrapPos $ \_ -> TAugmented BitOr }
    \.\.            { wrapPos $ \_ -> TInterval Through }
    \.\.\!          { wrapPos $ \_ -> TInterval Till }

{

wrapPos :: (String -> b) -> a -> String -> (a, b)
wrapPos f a s = (a, f s)

data Token = Newline
           -- Primitive symbols
           | Semicolon
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

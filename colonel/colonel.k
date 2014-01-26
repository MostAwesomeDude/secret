module COLONEL
    syntax Literal ::= Bool
                     | Char
                     | Float
                     | Int
                     | String

    syntax Object ::= Literal

    /*@
    Notes on syntax:
    * Expressions can be bracketed in parentheses for grouping.
    * Bare identifiers are expressions.
    * Literals are also expressions.
    */
    syntax Expr ::= "(" Expr ")"                        [bracket]
                  | Literal
                  | Id
                  | "if" "(" Expr ")" Expr "else" Expr  [strict(1)]
                  | Id ":=" Expr                        [right, strict(2)]
                  | "def" Id ":=" Expr                  [right, strict(2)]
                  | "var" Id ":=" Expr                  [right, strict(2)]
                  > Expr ";" Expr                       [left]

    configuration <T color="yellow">
        <k color="green"> $PGM:Expr </k>
        <variable color="red"> .Map </variable>
        <final color="blue"> .Map </final>
    </T>

    /*@
    Sequences of expressions are evaluated left-to-right.
    */
    rule [sequence]: First:Expr; Second:Expr => First ~> Second [structural]

    /*@
    If-expressions use an expression for a conditional, but require evaluation
    of the expression to a boolean.
    */
    rule [if-true]: if (true)  E else _ => E
    rule [if-false]: if (false) _ else E => E

    /*@
    Assignments are right-associative and return the assigned value.
    */
    rule [assign]: <k> L:Id := R => R ...</k>
                   <variable>... L |-> (_ => R) ...</variable>

    /*@
    Final assignments may not be overwritten.
    */
    rule [assign-def]: <k> def L:Id := R => R ...</k>
                       <final>... . => (L |-> R) ...</final>

    /*@
    Varying assignments may change over time.
    */
    rule [assign-var]: <k> var L:Id := R => R ...</k>
                       <variable>... . => (L |-> R) ...</variable>

    /*@
    Lone identifiers may be expressions.
    */
    rule [lookup-def]: <k> K:Id => V ...</k> <final>... K |-> V ...</final>
    rule [lookup-var]: <k> K:Id => V ...</k>
                       <variable>... K |-> V ...</variable>

    /*@
    Literals as expressions have no side effects; eliminate them.
    */
    rule L:Literal => .

    syntax KResult ::= Literal
endmodule

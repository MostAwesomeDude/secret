module COLONEL
    syntax Literal ::= Bool
                     | Char
                     | Float
                     | Int
                     | String

    /*@
    Expressions can be bracketed in parentheses for grouping.
    */
    syntax Expr ::= "(" Expr ")"  [bracket]

    configuration <T color="yellow">
        <k color="green"> $PGM:Expr </k>
        <variable color="red"> .Map </variable>
        <final color="blue"> .Map </final>
    </T>

    /*@
    Sequences of expressions are evaluated left-to-right.
    */
    syntax Expr ::= Expr ";" Expr [left]

    rule First:Expr; Second:Expr => First ~> Second

    /*@
    If-expressions use an expression for a conditional, but require evaluation
    of the expression to a boolean.
    */
    syntax Expr ::= "if" "(" Expr ")" Expr "else" Expr [strict(1)]

    rule if (true)  E else _ => E
    rule if (false) _ else E => E

    /*@
    Assignments are right-associative and return the assigned value.
    */
    syntax Expr ::= Id ":=" Expr [right, strict(2)]

    rule <k> L:Id := R => R ...</k> <variable>... L |-> (_ => R) ...</variable>

    /*@
    Final assignments may not be overwritten.
    */
    syntax Expr ::= "def" Id ":=" Expr [right, strict(2)]

    rule <k> def L:Id := R => R ...</k> <final>... . => (L |-> R) ...</final>

    /*@
    Varying assignments may change over time.
    */
    syntax Expr ::= "var" Id ":=" Expr [right, strict(2)]

    rule <k> var L:Id := R => R ...</k>
         <variable>... . => (L |-> R) ...</variable>

    /*@
    Lone identifiers may be expressions.
    */
    syntax Expr ::= Id

    rule <k> K:Id => V ...</k> <final>... K |-> V ...</final>
    rule <k> K:Id => V ...</k> <variable>... K |-> V ...</variable>

    /*@
    Literals are also expressions.
    */
    syntax Expr ::= Literal

    /*@
    Literals as expressions have no side effects; eliminate them.
    */
    rule L:Literal ~> E:Expr => E

    syntax KResult ::= Literal
endmodule

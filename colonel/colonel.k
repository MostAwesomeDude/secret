module COLONEL
    syntax Literal ::= Char
                     | Float
                     | Int
                     | String
    syntax Expr ::= Literal
                  | "(" Expr ")" [bracket]
endmodule

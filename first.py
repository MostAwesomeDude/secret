import sys

from terml.nodes import Term, termMaker as t
from ometa.boot import BootOMetaGrammar
from ometa.runtime import ParseError, expected, TreeTransformerBase
from ometa.grammar import TreeTransformerGrammar
from parsley import wrapGrammar

g = open("python.parsley").read()

def join(separator, seq):
    "A Twine-friendly string join."
    if not seq:
        return ''
    return seq[0].__class__(separator).join(seq)


class PythonParser(BootOMetaGrammar.makeGrammar(g, globals())):

    depth = 0

    parens = 0

    keywords = [
        "and",
        "as",
        "assert",
        "break",
        "class",
        "continue",
        "def",
        "del",
        "elif",
        "else",
        "except",
        "exec",
        "finally",
        "for",
        "from",
        "global",
        "if",
        "import",
        "in",
        "is",
        "lambda",
        "not",
        "or",
        "pass",
        "print",
        "raise",
        "return",
        "try",
        "while",
        "with",
        "yield",
    ]

    def __init__(self, *args, **kwargs):
        super(PythonParser, self).__init__(*args, **kwargs)
        self.indents = []

    def rule_until(self, rule, token):
        """
        Parse up until a given token, using the given rule.

        The token may be multiple characters or a single character.
        """

        m = self.input

        try:
            result = []
            while True:
                try:
                    s = self.input
                    for char in token:
                        v, e = self.exactly(char)
                    return result, e
                except ParseError:
                    self.input = s
                    v, e = self.apply(rule)
                    result.append(v)
        except ParseError, pe:
            self.input = m
            raise pe.withMessage(expected("%s until" % rule, token))

    def keyword_pred(self, first, second):
        return first + second in self.keywords

    def tuplify(self, head, tail, trailing):
        """
        Perform tuplification logic.
        """

        if tail:
            return t.Tuple(head, *tail)
        elif trailing:
            return t.Tuple(head)
        else:
            return head

    def argdicts(self, first, second):
        """
        Combine two dictionaries of arguments.
        """

        if not first and not second:
            return None

        raise NotImplementedError()

    def po(self):
        self.parens += 1

    def pc(self):
        self.parens -= 1

    def primary(self, atom, trailers):
        """
        Re-nest a series of trailers on an atom.
        """

        if trailers:
            # Ugh. This trickery is necessary because Terms are hard to unpack
            # and repack.
            for term in trailers:
                repacked = (atom,) + term.args[1:]
                atom = Term(term.tag, term.data, repacked, term.span)

        return atom


class PythonWriter(object):

    indent = 0

    def __init__(self):
        self.parts = []

    def write(self, statements):
        for statement in statements:
            self._write(statement)

    def _write(self, t):
        if isinstance(t, list):
            for s in t:
                self._write(s)
        else:
            self.term(t)

    def start_line(self):
        self.parts.append(" " * self.indent)

    def end_line(self):
        self.parts.append("\n")

    def term(self, t):
        getattr(self, "term_%s" % t.tag.name)(t)

    def term_Assign(self, t):
        self.start_line()
        # XXX doesn't support lvalue unpacks
        self.term(t.args[0].args[0])
        self.parts.append(" = ")
        self.term(t.args[1])
        self.end_line()

    def term_Attribute(self, t):
        self.term(t.args[0])
        self.parts.append(".")
        self.parts.append(t.args[1].data)

    def term_Call(self, t):
        self.term(t.args[0])
        # XXX doesn't support actually emitting parameters to calls yet
        self.parts.append("()")

    def term_Class(self, t):
        self.start_line()
        self.parts.append("class ")
        self.parts.append(t.args[0].data)
        self.parts.append("(")
        for parent in t.args[1:-1]:
            self.term(parent)
        self.parts.append("):")
        self.end_line()
        self.indent += 1
        for statement in t.args[-1].args:
            self.term(statement)
        self.indent -= 1

    def term_Def(self, t):
        self.start_line()
        self.parts.append("def ")
        self.parts.append(t.args[0].data)
        # XXX doesn't insert parameters
        self.parts.append("():")
        self.end_line()
        self.indent += 1
        for statement in t.args[-1].args:
            self.term(statement)
        self.indent -= 1

    def term_If(self, t):
        self.start_line()
        self.parts.append("if ")
        self.term(t.args[0])
        self.parts.append(":")
        self.end_line()
        self.indent += 1
        for statement in t.args[1].args:
            self.term(statement)
        self.indent -= 1
        # XXX else?

    def term_Name(self, t):
        self.parts.append(t.args[0].data)

    def term_Not(self, t):
        self.parts.append("(not ")
        self.term(t.args[0])
        self.parts.append(")")

    def term_Num(self, t):
        self.parts.append(str(t.args[0].data))

    def term_Pass(self, t):
        self.start_line()
        # I think we can end the line ourselves, thanks. :3
        self.parts.append("pass\n")

    def term_Return(self, t):
        self.start_line()
        self.parts.append("return ")
        self.term(t.args[0])
        self.end_line()

    def term_Str(self, t):
        if t.args[0].data:
            self.parts.append(" %s" % (t.args[0].data,))
        self.parts.append(repr(t.args[1].data))

    def term_Subscript(self, t):
        self.term(t.args[0])
        self.parts.append("[")
        self.term(t.args[1])
        self.parts.append("]")

    def term_Tuple(self, t):
        self.parts.append("(")
        for element in t.args:
            self.term(element)
            self.parts.append(",")
        self.parts.append(")")

unparser = r"""
assign_lhs_part = (Attribute(assign_lhs_part:o @attr) --> $o.$attr
                  |Subscript(assign_lhs_part:o @sub) --> $o[$sub]
                  |Call(assign_lhs_part:o @args) --> $o($args)
                  |Name(@n) --> n
                  )
assign_lhs = Tuple(assign_lhs_part*:parts) -> ', '.join(parts)
multi_assign = assign_lhs+:lhs -> " = ".join(lhs)
Assign([multi_assign:lhss] @rhs) --> $lhss = $rhs

Attribute(@expr @name) --> $expr.$name

Call(@expr null) --> $expr()
Call(@expr @args) --> $expr($args)

Class(@name null @suite) {{{
class $name:
    $suite
}}}
Class(@name (Name(@p) | cls_parents:p) @suite) {{{
class $name($p):
    $suite
}}}
cls_parents [transform*:es] -> ", ".join(es)

Def(@name @params @suite) {{{
def $name($p):
    $suite
}}}
elifs = [@ele @elthen] {{{
elif $ele:
    $elthen
}}}
If(@test @consq [elifs*]:els null) {{{
if $test:
    $consq
$els
}}}
If(@test @consq [elifs*]:els @alt) {{{
if $test:
    $consq
$els
else:
    $alt
}}}

Name(@n) -> n
Not(@expr) --> (not $expr)
Num(@n) -> str(n)
Pass() --> pass
Return(@expr) --> return $expr
Str(@prefix @s) -> prefix + repr(s)
Subscript(@expr @sub) --> $expr[$sub]

Tuple(@single) --> ($single,)
Tuple(transform*:elts) -> '(' + ', '.join(elts) + ')'

"""

PythonUnparser = TreeTransformerGrammar.makeGrammar(
    unparser, globals(),
    'PythonUnparser', superclass=TreeTransformerBase)



if __name__ == "__main__":
    f = open(sys.argv[1]).read()
    g = wrapGrammar(PythonParser)
    stmts = g(f).file_input()
    from pprint import pprint
    pprint(stmts)
    print PythonUnparser.transform(stmts[0][0])[0]
    # pw = PythonWriter()
    # pw.write(stmts)
    # print pw.parts
    # print "".join(pw.parts)











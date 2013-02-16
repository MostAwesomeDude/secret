import sys

from terml.nodes import Term, termMaker as t
from ometa.boot import BootOMetaGrammar
from ometa.runtime import ParseError, expected
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

    def term_Name(self, t):
        self.parts.append(t.args[0].data)

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

    def term_Tuple(self, t):
        self.parts.append("(")
        for element in t.args:
            self.term(element)
            self.parts.append(",")
        self.parts.append(")")


if __name__ == "__main__":
    f = open(sys.argv[1]).read()
    g = wrapGrammar(PythonParser)
    stmts = g(f).file_input()
    from pprint import pprint
    pprint(stmts)

    pw = PythonWriter()
    pw.write(stmts)
    print pw.parts
    print "".join(pw.parts)

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


class Parser(BootOMetaGrammar.makeGrammar(g, globals())):

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
        super(Parser, self).__init__(*args, **kwargs)
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


if __name__ == "__main__":
    import sys
    f = open(sys.argv[1]).read()
    g = wrapGrammar(Parser)
    stmts = g(f).file_input()
    from pprint import pprint
    pprint(stmts)

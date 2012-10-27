from terml.nodes import Term, termMaker as t
from ometa.boot import BootOMetaGrammar
from ometa.runtime import ParseError, expected

g = open("python.parsley").read()

def join(separator, seq):
    "A Twine-friendly string join."
    if not seq:
        return ''
    return seq[0].__class__(separator).join(seq)


class Parser(BootOMetaGrammar.makeGrammar(g, globals())):

    depth = 0

    def _apply(self, *args):
        self.depth += 1
        #print " " * self.depth, "apply", args[1:]
        try:
            rv = super(Parser, self)._apply(*args)
            #print " " * self.depth, "success", args[1:], rv
            return rv
        except:
            #print " " * self.depth, "failed", args[1:]
            raise
        finally:
            self.depth -= 1

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
        rule = rule[0]

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
            raise ParseError(self.input.data, pe[0],
                    expected("%s until" % rule, token))

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
    try:
        stmts, stuff = Parser(f).rule_file_input()
        error = ParseError(f, *stuff)
        from pprint import pprint
        pprint(stmts)
        print error.formatError()
    except ParseError as error:
        print error.formatError()

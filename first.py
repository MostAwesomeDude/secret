import sys, itertools

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


class PrecedenceTemplate(object):
    def __init__(self, prec, contents):
        self.prec = prec
        self.contents = contents

    def __str__(self):
        parts = []
        for item in self.contents:
            if isinstance(item, basestring):
                parts.append(item)
            else:
                prec = getattr(item, 'prec', None)
                if prec is None:
                    prec = -float('inf')
                if prec > self.prec:
                    parts.append("(%s)" % (item,))
                else:
                    parts.append(str(item))
        return ''.join(parts)

    def __repr__(self):
        return "<PT %s %r>" % (self.prec, self.contents)


class PrecedenceTransformer(TreeTransformerBase):
    nextPrecedence = None

    def rule_prec(self, prec):
        self.nextPrecedence = prec
        return None, self.input.nullError()

    def stringtemplate(self, template, vals):
        output = []
        for chunk in template.args:
            if chunk.tag.name == ".String.":
                output.append(chunk.data)
            elif chunk.tag.name == "QuasiExprHole":
                v = vals[chunk.args[0].data]
                output.append(v)
            else:
                raise TypeError("didn't expect %r in string template" % chunk)

        pt = PrecedenceTemplate(self.nextPrecedence, output)
        self.nextPrecedence = None
        return pt

PythonExpressionUnparser = TreeTransformerGrammar.makeGrammar(
    open("expression_unparser.parsley").read(), globals(),
    'PythonExpressionUnparser', superclass=PrecedenceTransformer)

PythonStatementUnparser = TreeTransformerGrammar.makeGrammar(
    open("statement_unparser.parsley").read(), globals(),
    'PythonStatementUnparser', superclass=TreeTransformerBase)


if __name__ == "__main__":
    from terml.parser import parseTerm as term
    # f = open(sys.argv[1]).read()
    g = wrapGrammar(PythonParser)
    stmts = g(sys.argv[1] + '\n').file_input()
    from pprint import pprint
    pprint(stmts)
    pt = PythonExpressionUnparser.transform(stmts)[0]
    print pt

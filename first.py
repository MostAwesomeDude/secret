# Copyright (C) 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.
import sys

from terml.nodes import Term, termMaker as t
from ometa.runtime import ParseError, expected, TreeTransformerBase, OMetaBase
from ometa.grammar import OMeta, TreeTransformerGrammar
from parsley import wrapGrammar

g = open("python.parsley").read()

def join(separator, seq):
    "A Twine-friendly string join."
    if not seq:
        return ''
    return seq[0].__class__(separator).join(seq)


class PythonParser(OMeta.makeGrammar(g, name="PythonParser")
                   .createParserClass(OMetaBase, globals())):

    depth = 0

    parens = 0

    keywords = [
        "and",
        "as",
        "assert",
        "break",
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
        "object",
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

    @staticmethod
    def stringify(pieces):
        """
        Perform string concatentation.
        """

        unicode_needed = any("u" in piece[0].lower() for piece in pieces
                if piece[0])

        l = []

        for p, s in pieces:
            if unicode_needed and "u" not in p:
                s = s.decode("ascii")
            l.append(s)

        prefix = "u" if unicode_needed else None
        return t.Str(prefix, "".join(l))


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

        first = first or []
        second = second or []

        return first + second or None

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
                atom = Term(term.tag, term.data, repacked)
        return atom


class PrecedenceTemplate(object):
    def __init__(self, prec, contents):
        self.prec = prec or 0
        self.contents = contents

    def __str__(self):
        parts = []
        for item in self.contents:
            if isinstance(item, basestring):
                parts.append(item)
            else:
                prec = getattr(item, 'prec', None) or 0
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
        return pt, None


Desugarer = TreeTransformerGrammar.makeGrammar(
    open("desugar_augmented_assignment.parsley").read(),
    "Desugarer").createParserClass(TreeTransformerBase, {"t": t})


PythonExpressionUnparser = TreeTransformerGrammar.makeGrammar(
    open("expression_unparser.parsley").read(),
    'PythonExpressionUnparser').createParserClass(PrecedenceTransformer, globals())


PythonStatementUnparser = TreeTransformerGrammar.makeGrammar(
    open("statement_unparser.parsley").read(),
    'PythonStatementUnparser').createParserClass(TreeTransformerBase, globals())


if __name__ == "__main__":
    from terml.parser import parseTerm as term
    f = open(sys.argv[1]).read()
    g = wrapGrammar(PythonParser)
    # stmts = g(sys.argv[1] + '\n').file_input()
    stmts = g(f).file_input()
    from pprint import pprint
    pprint(stmts)
#    import pdb; pdb.set_trace()
    ds = Desugarer.transform(stmts)[0]
    print ds
    pt = PythonStatementUnparser.transform(ds)[0]
    print pt

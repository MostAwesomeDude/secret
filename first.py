from terml.nodes import termMaker as t
from ometa.grammar import OMeta

g = open("python.parsley").read()

class Parser(OMeta.makeGrammar(g, globals())):

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

    def keyword_pred(self, first, second):
        return first + second in self.keywords

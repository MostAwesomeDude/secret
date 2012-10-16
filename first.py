from terml.nodes import termMaker as t
from ometa.grammar import OMeta
from ometa.runtime import ParseError

g = open("python.parsley").read()

class Parser(OMeta.makeGrammar(g, globals())):

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


if __name__ == "__main__":
    import sys
    f = open(sys.argv[1]).read()
    try:
        stmts, stuff = Parser(f).rule_file_input()
        error = ParseError(f, *stuff)
        print stmts
        print error.formatError()
    except ParseError as error:
        print error.formatError()

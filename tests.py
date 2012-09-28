from unittest import TestCase

from terml.nodes import termMaker as t

from first import Parser

class TC(TestCase):

    def succeed(self, i, o):
        p = Parser(i)
        result, errors = getattr(p, "rule_%s" % self.rule)()
        self.assertEqual(result, o)

class TestStringLiteral(TC):

    rule = "stringliteral"

    def test_empty_single_quotes(self):
        i = "''"
        o = t.Str(None, "")
        self.succeed(i, o)

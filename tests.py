from unittest import TestCase

from terml.nodes import termMaker as t

from first import Parser

class TC(TestCase):

    def succeed(self, i, o):
        p = Parser(i)
        result, errors = getattr(p, "rule_%s" % self.rule)()
        self.assertEqual(result, o)


class TestInteger(TC):

    rule = "integer"

    def test_zero(self):
        i = "0"
        o = t.Num(0)
        self.succeed(i, o)

    def test_one(self):
        i = "1"
        o = t.Num(1)
        self.succeed(i, o)

    def test_lue(self):
        i = "42"
        o = t.Num(42)
        self.succeed(i, o)

    def test_bin(self):
        i = "0b101"
        o = t.Num(5)
        self.succeed(i, o)

    def test_oct(self):
        i = "0o101"
        o = t.Num(65)
        self.succeed(i, o)

    def test_hex(self):
        i = "0x101"
        o = t.Num(257)
        self.succeed(i, o)


class TestStringLiteral(TC):

    rule = "stringliteral"

    def test_empty_single_quotes(self):
        i = "''"
        o = t.Str(None, "")
        self.succeed(i, o)

    def test_single_single_quotes(self):
        i = "'a'"
        o = t.Str(None, "a")
        self.succeed(i, o)

    def test_plural_single_quotes(self):
        i = "'abc'"
        o = t.Str(None, "abc")
        self.succeed(i, o)

    def test_empty_double_quotes(self):
        i = '""'
        o = t.Str(None, "")
        self.succeed(i, o)

    def test_single_double_quotes(self):
        i = '"a"'
        o = t.Str(None, "a")
        self.succeed(i, o)

    def test_plural_double_quotes(self):
        i = '"abc"'
        o = t.Str(None, "abc")
        self.succeed(i, o)


class TestBoolOp(TC):

    rule = "boolop"

    def test_and(self):
        i = "and"
        o = t.And()
        self.succeed(i, o)


class TestOperator(TC):

    rule = "operator"

    def test_add(self):
        i = "+"
        o = t.Add()
        self.succeed(i, o)


class TestExpr(TC):

    rule = "expr"

    def test_boolop_num(self):
        i = "1 and 2"
        o = t.BoolOp(t.And(), t.Num(1), t.Num(2))
        self.succeed(i, o)

    def test_unaryop_invert_num(self):
        i = "~0"
        o = t.UnaryOp(t.Invert(), t.Num(0))
        self.succeed(i, o)

    def test_unaryop_not_num(self):
        i = "not 0"
        o = t.UnaryOp(t.Not(), t.Num(0))
        self.succeed(i, o)

    def test_nested_arith_left(self):
        i = "(1 + 2) + 3"
        o = t.BinOp(t.Add(), t.BinOp(t.Add(), t.Num(1), t.Num(2)), t.Num(3))
        self.succeed(i, o)

    def test_nested_arith_right(self):
        i = "1 + (2 + 3)"
        o = t.BinOp(t.Add(), t.Num(1), t.BinOp(t.Add(), t.Num(2), t.Num(3)))
        self.succeed(i, o)


class TestSet(TC):

    rule = "set"

    def test_set_literal_single(self):
        i = "{1}"
        o = t.Set(t.Num(1))
        self.succeed(i, o)

    def test_set_literal_single_trailing(self):
        i = "{1,}"
        o = t.Set(t.Num(1))
        self.succeed(i, o)

    def test_set_literal_plural(self):
        i = "{1,2}"
        o = t.Set(t.Num(1), t.Num(2))
        self.succeed(i, o)

    def test_set_literal_plural_trailing(self):
        i = "{1,2,}"
        o = t.Set(t.Num(1), t.Num(2))
        self.succeed(i, o)


class TestSlice(TC):

    rule = "slice"

    def test_ellipsis(self):
        i = "..."
        o = t.Ellipsis()
        self.succeed(i, o)

from unittest import TestCase

from ometa.runtime import ParseError
from terml.nodes import termMaker as t

from first import Parser


class TC(TestCase):

    def fail(self, i):
        p = Parser(i)
        m = getattr(p, "rule_%s" % self.rule)
        self.assertRaises(ParseError, m)

    def succeed(self, i, o):
        p = Parser(i)
        result, errors = getattr(p, "rule_%s" % self.rule)()
        self.assertEqual(result, o)


class TestIdentifier(TC):

    rule = "identifier"

    def test_foo(self):
        i = "foo"
        o = t.Identifier("foo")
        self.succeed(i, o)

    def test_lambda(self):
        i = "lambda"
        self.fail(i)


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


class TestParenthForm(TC):

    rule = "parenth_form"

    def test_tuple_empty(self):
        i = "()"
        o = t.Tuple()
        self.succeed(i, o)

    def test_tuple_single(self):
        i = "(1,)"
        o = t.Tuple(t.Num(1))
        self.succeed(i, o)

    def test_tuple_pair(self):
        i = "(1,2)"
        o = t.Tuple(t.Num(1), t.Num(2))
        self.succeed(i, o)


class TestSlicing(TC):

    rule = "slicing"

    def test_ellipsis(self):
        i = "..."
        o = t.Ellipsis()
        self.succeed(i, o)


class TestCall(TC):

    rule = "call"

    def test_call_identifier_empty(self):
        i = "call()"
        o = t.Call(t.Identifier("call"), t.Arguments())
        self.succeed(i, o)

    def test_call_identifier_args(self):
        i = "call(arg)"
        o = t.Call(t.Identifier("call"), t.Arguments(t.Identifier("arg")))
        self.succeed(i, o)


class TestDictDisplay(TC):

    rule = "dict_display"

    def test_dict_display_empty(self):
        i = "{}"
        o = t.Dict()
        self.succeed(i, o)

    def test_dict_display_single(self):
        i = "{1:2}"
        o = t.Dict(t.Pair(t.Num(1), t.Num(2)))
        self.succeed(i, o)

    def test_dict_display_single_trailing(self):
        i = "{1:2,}"
        o = t.Dict(t.Pair(t.Num(1), t.Num(2)))
        self.succeed(i, o)

    def test_dict_display_plural(self):
        i = "{1:2,3:4}"
        o = t.Dict(t.Pair(t.Num(1), t.Num(2)), t.Pair(t.Num(3), t.Num(4)))
        self.succeed(i, o)

    def test_dict_display_plural_trailing(self):
        i = "{1:2,3:4,}"
        o = t.Dict(t.Pair(t.Num(1), t.Num(2)), t.Pair(t.Num(3), t.Num(4)))
        self.succeed(i, o)


class TestSetDisplay(TC):

    rule = "set_display"

    def test_set_display_single(self):
        i = "{1}"
        o = t.Set(t.Num(1))
        self.succeed(i, o)

    def test_set_display_single_trailing(self):
        i = "{1,}"
        o = t.Set(t.Num(1))
        self.succeed(i, o)

    def test_set_display_plural(self):
        i = "{1,2}"
        o = t.Set(t.Num(1), t.Num(2))
        self.succeed(i, o)

    def test_set_display_plural_trailing(self):
        i = "{1,2,}"
        o = t.Set(t.Num(1), t.Num(2))
        self.succeed(i, o)


class TestPower(TC):

    rule = "power"

    def test_power_num(self):
        i = "2**3"
        o = t.Pow(t.Num(2), t.Num(3))
        self.succeed(i, o)


class TestUExpr(TC):

    rule = "u_expr"

    def test_u_expr_invert_num(self):
        i = "~0"
        o = t.Invert(t.Num(0))
        self.succeed(i, o)


class TestMExpr(TC):

    rule = "m_expr"

    def test_m_expr_mult_num(self):
        i = "3 * 4"
        o = t.Mult(t.Num(3), t.Num(4))
        self.succeed(i, o)


class TestAExpr(TC):

    rule = "a_expr"

    def test_u_expr_add_identifier_left(self):
        i = "a + 1"
        o = t.Add(t.Identifier("a"), t.Num(1))
        self.succeed(i, o)

    def test_u_expr_add_identifier_right(self):
        i = "1 + a"
        o = t.Add(t.Num(1), t.Identifier("a"))
        self.succeed(i, o)


class TestNotTest(TC):

    rule = "not_test"

    def test_unaryop_not_num(self):
        i = "not 0"
        o = t.Not(t.Num(0))
        self.succeed(i, o)


class TestAndTest(TC):

    rule = "and_test"

    def test_and_test_num(self):
        i = "1 and 2"
        o = t.BoolOp(t.Num(1), t.And(), t.Num(2))
        self.succeed(i, o)


class TestPassStmt(TC):

    rule = "pass_stmt"

    def test_pass(self):
        i = "pass"
        o = t.Pass()
        self.succeed(i, o)


class TestBreakStmt(TC):

    rule = "break_stmt"

    def test_break(self):
        i = "break"
        o = t.Break()
        self.succeed(i, o)


class TestContinueStmt(TC):

    rule = "continue_stmt"

    def test_continue(self):
        i = "continue"
        o = t.Continue()
        self.succeed(i, o)


class TestIfStmt(TC):

    rule = "if_stmt"

    def test_if_true_pass(self):
        i = "if True:\n pass\n"
        o = t.If(t.Identifier("True"), [t.Pass()], [], None)
        self.succeed(i, o)

    def test_if_true_else_pass(self):
        i = "if True:\n pass\nelse:\n pass\n"
        o = t.If(t.Identifier("True"), [t.Pass()], [], [t.Pass()])
        self.succeed(i, o)

    def test_if_true_elif_else_pass(self):
        i = "if True:\n pass\nelif False:\n pass\nelse:\n pass\n"
        o = t.If(t.Identifier("True"),
                 [t.Pass()],
                 [(t.Identifier("False"), [t.Pass()])],
                 [t.Pass()])
        self.succeed(i, o)


class TestWhileStmt(TC):

    rule = "while_stmt"

    def test_while_true_pass(self):
        i = "while True:\n pass\n"
        o = t.While(t.Identifier("True"), [t.Pass()])
        self.succeed(i, o)


"""
class TestExpr(TC):

    rule = "expr"

    def test_nested_arith_right(self):
        i = "1 + (2 + 3)"
        o = t.BinOp(t.Add(), t.Num(1), t.BinOp(t.Add(), t.Num(2), t.Num(3)))
        self.succeed(i, o)

    def test_attr_literal(self):
        i = "an.attr"
        o = t.Attribute(t.Identifier("an"), t.Identifier("attr"))
        self.succeed(i, o)

    def test_if_else_literals(self):
        i = "1 if 2 else 3"
        o = t.IfExp(t.Num(2), t.Num(1), t.Num(3))
        self.succeed(i, o)

    def test_if_else_complex(self):
        i = "('this' if it.succeeds() else 'that')"
        o = t.IfExp(t.Call(t.Attribute(t.Identifier("it"),
            t.Identifier("succeeds")), t.Arguments()), t.Str("this"),
            t.Str("that"))
        self.succeed(i, o)

    def test_lambda_id(self):
        i = "lambda id: id"
        o = t.Lambda(t.Arguments(t.Identifier("id")), t.Identifier("id"))
        self.succeed(i, o)

    def test_method(self):
        i = "(test.method())"
        o = t.Call(t.Attribute(t.Identifier("test"), t.Identifier("method")),
                t.Arguments())
        self.succeed(i, o)


class TestArguments(TC):

    rule = "arguments"

    def test_arguments_empty(self):
        i = ""
        o = t.Arguments()
        self.succeed(i, o)

    def test_arguments_single(self):
        i = "foo"
        o = t.Arguments(t.Identifier("foo"))
        self.succeed(i, o)

    def test_arguments_single_trailing(self):
        i = "foo,"
        o = t.Arguments(t.Identifier("foo"))
        self.succeed(i, o)

    def test_arguments_plural(self):
        i = "foo,bar"
        o = t.Arguments(t.Identifier("foo"), t.Identifier("bar"))
        self.succeed(i, o)

    def test_arguments_plural_trailing(self):
        i = "foo,bar,"
        o = t.Arguments(t.Identifier("foo"), t.Identifier("bar"))
        self.succeed(i, o)
        """

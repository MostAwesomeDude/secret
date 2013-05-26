from unittest import TestCase

from ometa.runtime import ParseError
from terml.nodes import termMaker as t

from first import PythonParser as Parser


class TC(TestCase):

    # The discriminant from the quadratic formula. Generally useful as a test
    # expression.
    discriminant_string = "b ** 2 - 4 * a * c"
    discriminant = t.Sub(t.Pow(t.Name("b"), t.Num(2)), t.Mul(t.Mul(t.Num(4),
        t.Name("a")), t.Name("c")))

    def fail(self, i):
        p = Parser(i)
        m = getattr(p, "rule_%s" % self.rule)
        self.assertRaises(ParseError, m)

    def succeed(self, i, o):
        p = Parser(i)
        result, errors = getattr(p, "rule_%s" % self.rule)()
        self.assertEqual(result, o)


class TestComment(TC):

    rule = "comment"

    def test_comment(self):
        i = "#test"
        o = "test"
        self.succeed(i, o)

    def test_comment_newline(self):
        i = "#test\n"
        o = "test"
        self.succeed(i, o)


class TestIdentifier(TC):

    rule = "identifier"

    def test_foo(self):
        i = "foo"
        o = t.Name("foo")
        self.succeed(i, o)

    def test_dunder(self):
        i = "__file__"
        o = t.Name("__file__")
        self.succeed(i, o)

    def test_dunder_keyword(self):
        i = "__class__"
        o = t.Name("__class__")
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

    def test_raw_empty(self):
        i = 'r""'
        o = t.Str(None, "")
        self.succeed(i, o)

    def test_raw(self):
        i = 'r"abc"'
        o = t.Str(None, "abc")
        self.succeed(i, o)

    def test_raw_slash(self):
        i = 'r"abc\n"'
        o = t.Str(None, "abc\\n")
        self.succeed(i, o)

    def test_raw_quote(self):
        i = 'r"abc\""'
        o = t.Str(None, "abc\\\"")
        self.succeed(i, o)

    def test_str_c_style(self):
        i = '"abc" "def"'
        o = t.Str(None, "abcdef")
        self.succeed(i, o)

    def test_str_long_single(self):
        i = "'''single'''"
        o = t.Str(None, "single")
        self.succeed(i, o)

    def test_str_long_double(self):
        i = '"""double"""'
        o = t.Str(None, "double")
        self.succeed(i, o)

    def test_str_long_single_newline_escape(self):
        i = "'''single\n'''"
        o = t.Str(None, "single\n")
        self.succeed(i, o)

    def test_str_long_single_raw(self):
        i = "r'''single\n'''"
        o = t.Str(None, "single\\n")
        self.succeed(i, o)

    def test_longstring_docstring(self):
        i = '''"""
        Docstring
        """'''
        o = t.Str(None, "\n        Docstring\n        ")
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

    def test_tuple_empty_newline(self):
        i = "(\n)"
        o = t.Tuple()
        self.succeed(i, o)

    def test_expr_num(self):
        i = "(3)"
        o = t.Num(3)
        self.succeed(i, o)

    def test_m_expr_mult_num(self):
        i = "(3 * 4)"
        o = t.Mul(t.Num(3), t.Num(4))
        self.succeed(i, o)

    def test_a_expr_add_identifier_left(self):
        i = "(a + 1)"
        o = t.Add(t.Name("a"), t.Num(1))
        self.succeed(i, o)

    def test_a_expr_add_identifier_right(self):
        i = "(1 + a)"
        o = t.Add(t.Num(1), t.Name("a"))
        self.succeed(i, o)

    def test_and_test_num(self):
        i = "(1 and 2)"
        o = t.And(t.Num(1), t.Num(2))
        self.succeed(i, o)

    def test_if_else_literals(self):
        i = "(1 if 2 else 3)"
        o = t.IfExp(t.Num(2), t.Num(1), t.Num(3))
        self.succeed(i, o)

    def test_attr_literal(self):
        i = "(an.attr)"
        o = t.Attribute(t.Name("an"), "attr")
        self.succeed(i, o)

    def test_quasiliteral(self):
        i = "(just`a test`)"
        o = t.Quasi(t.Name("just"), "a test")
        self.succeed(i, o)

    def test_method(self):
        i = "(test.method())"
        o = t.Call(t.Attribute(t.Name("test"), "method"), None)
        self.succeed(i, o)

    def test_expr_associativity_left(self):
        i = "(a * b + c)"
        o = t.Add(t.Mul(t.Name("a"), t.Name("b")), t.Name("c"))
        self.succeed(i, o)

    def test_expr_associativity_right(self):
        i = "(a + b * c)"
        o = t.Add(t.Name("a"), t.Mul(t.Name("b"), t.Name("c")))
        self.succeed(i, o)

    def test_nested_arith_left(self):
        i = "((1 + 2) + 3)"
        o = t.Add(t.Add(t.Num(1), t.Num(2)), t.Num(3))
        self.succeed(i, o)

    def test_nested_arith_right(self):
        i = "(1 + (2 + 3))"
        o = t.Add(t.Num(1), t.Add(t.Num(2), t.Num(3)))
        self.succeed(i, o)

    def test_expr_discriminant(self):
        i = "(%s)" % self.discriminant_string
        o = self.discriminant
        self.succeed(i, o)

    def test_if_else_complex(self):
        i = "('this' if it.succeeds() else 'that')"
        o = t.IfExp(
                t.Call(t.Attribute(t.Name("it"), "succeeds"), None),
                t.Str(None, "this"),
                t.Str(None, "that"))
        self.succeed(i, o)


class TestPrimary(TC):

    rule = "primary"

    def test_attr_single(self):
        i = "an.attr"
        o = t.Attribute(t.Name("an"), "attr")
        self.succeed(i, o)

    def test_ellipsis(self):
        i = "x[...]"
        o = t.Subscript(t.Name("x"), t.Ellipsis())
        self.succeed(i, o)

    def test_call_identifier_empty(self):
        i = "call()"
        o = t.Call(t.Name("call"), None)
        self.succeed(i, o)

    def test_call_identifier_args(self):
        i = "call(arg)"
        o = t.Call(t.Name("call"),
                t.Arguments([t.Name("arg")], None, None))
        self.succeed(i, o)

    def test_call_identifier_kwargs(self):
        i = "call(kwarg=x)"
        o = t.Call(t.Name("call"),
                t.Arguments(None, [t.Pair("kwarg", t.Name("x"))], None))
        self.succeed(i, o)

    def test_call_identifier_expr(self):
        i = "sqrt(%s)" % self.discriminant_string
        o = t.Call(t.Name("sqrt"),
                t.Arguments([self.discriminant], None, None))
        self.succeed(i, o)

    def test_quasi_empty(self):
        i = "quasi``"
        o = t.Quasi(t.Name("quasi"), "")
        self.succeed(i, o)

    def test_quasi(self):
        i = "quasi`stuff`"
        o = t.Quasi(t.Name("quasi"), "stuff")
        self.succeed(i, o)


class TestKeywordItem(TC):

    rule = "keyword_item"

    def test_keyword_item_pair(self):
        i = "a=b"
        o = t.Pair("a", t.Name("b"))
        self.succeed(i, o)


class TestConditionalExpression(TC):

    rule = "conditional_expression"

    def test_if_else_literals(self):
        i = "1 if 2 else 3"
        o = t.IfExp(t.Num(2), t.Num(1), t.Num(3))
        self.succeed(i, o)


class TestExpressionList(TC):

    rule = "expression_list"

    def test_expression_list_num(self):
        i = "0"
        o = t.Num(0)
        self.succeed(i, o)

    def test_expression_list_str(self):
        i = '"PythonParser"'
        o = t.Str(None, "PythonParser")
        self.succeed(i, o)

    def test_expression_list_call_kwargs(self):
        i = 'f(g, k="s")'
        o = t.Call(t.Name("f"),
                t.Arguments([t.Name("g")], [t.Pair("k", t.Str(None, "s"))],
                    None))
        self.succeed(i, o)


class TestEllipsis(TC):

    rule = "ellipsis"

    def test_ellipsis(self):
        i = "..."
        o = t.Ellipsis()
        self.succeed(i, o)


class TestArgList(TC):

    rule = "arg_list"

    def test_args_positional_single(self):
        i = "arg"
        o = t.Arguments([t.Name("arg")], None, None)
        self.succeed(i, o)

    def test_args_positional_single_trailing(self):
        i = "arg,"
        o = t.Arguments([t.Name("arg")], None, None)
        self.succeed(i, o)

    def test_args_plural(self):
        i = "foo,bar"
        o = t.Arguments([t.Name("foo"), t.Name("bar")], None, None)
        self.succeed(i, o)

    def test_args_plural_trailing(self):
        i = "foo,bar,"
        o = t.Arguments([t.Name("foo"), t.Name("bar")], None, None)
        self.succeed(i, o)

    def test_args_keyword_args(self):
        i = "foo=bar"
        o = t.Arguments(None, [t.Pair("foo", t.Name("bar"))], None)
        self.succeed(i, o)

    def test_args_positional_and_keyword(self):
        i = "foo, bar=baz"
        o = t.Arguments([t.Name("foo")], [t.Pair("bar", t.Name("baz"))], None)
        self.succeed(i, o)

    def test_args_star_args(self):
        i = "*args"
        o = t.Arguments(None, None, t.Name("args"))
        self.succeed(i, o)


class TestListDisplay(TC):

    rule = "list_display"

    def test_list_display_empty(self):
        i = "[]"
        o = t.List()
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

    def test_dict_display_plural_newline_trailing(self):
        """
        A practical dictionary reflecting commonly-used indentation.
        """

        i = "{\n 1:2,\n 3:4,\n}"
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
        o = t.Set(t.Tuple(t.Num(1)))
        self.succeed(i, o)

    def test_set_display_plural(self):
        i = "{1,2}"
        o = t.Set(t.Tuple(t.Num(1), t.Num(2)))
        self.succeed(i, o)

    def test_set_display_plural_trailing(self):
        i = "{1,2,}"
        o = t.Set(t.Tuple(t.Num(1), t.Num(2)))
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

    def test_u_expr_invert_num_newline(self):
        i = "~\n0"
        self.fail(i)


class TestNotTest(TC):

    rule = "not_test"

    def test_unaryop_not_num(self):
        i = "not 0"
        o = t.Not(t.Num(0))
        self.succeed(i, o)


class TestLambdaForm(TC):

    rule = "lambda_form"

    def test_lambda_id(self):
        i = "lambda id: id"
        o = t.Lambda(t.Parameters([t.Name("id")]), t.Name("id"))
        self.succeed(i, o)


class TestAssignmentStmt(TC):

    rule = "assignment_stmt"

    def test_assign_simple(self):
        i = "x = 0"
        o = t.Assign([t.Name("x")], t.Num(0))
        self.succeed(i, o)

    def test_assign_call(self):
        i = "x = call()"
        o = t.Assign([t.Name("x")], t.Call(t.Name("call"), None))
        self.succeed(i, o)


class TestAugmentedAssignmentStmt(TC):

    rule = "augmented_assignment_stmt"

    def test_augassign_simple(self):
        i = "x += 1"
        o = t.AugAssign(t.Name("x"), "+=", t.Num(1))
        self.succeed(i, o)


class TestPassStmt(TC):

    rule = "pass_stmt"

    def test_pass(self):
        i = "pass"
        o = t.Pass()
        self.succeed(i, o)


class TestReturnStmt(TC):

    rule = "return_stmt"

    def test_return(self):
        i = "return"
        o = t.Return(None)
        self.succeed(i, o)

    def test_return_add(self):
        i = "return x + y"
        o = t.Return(t.Add(t.Name("x"), t.Name("y")))
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

class TestImportStmt(TC):

    rule = "import_stmt"

    def test_import_sys(self):
        i = "import sys"
        o = t.Import([t.As(t.Module("sys"), None)])
        self.succeed(i, o)

    def test_import_dotted(self):
        i = "import os.path"
        o = t.Import([t.As(t.Module("os.path"), None)])
        self.succeed(i, o)

    def test_import_as(self):
        i = "import os.path as path"
        o = t.Import([t.As(t.Module("os.path"), t.Name("path"))])
        self.succeed(i, o)


class TestStatement(TC):

    rule = "statement"

    def test_statement_compound_while(self):
        i = "while True: pass\n"
        o = [t.While(t.Name("True"), [t.Pass()])]
        self.succeed(i, o)

    def test_statement_compound_while_multiline(self):
        i = """while True:
            pass
        """
        o = [t.While(t.Name("True"), [t.Pass()])]
        self.succeed(i, o)

    def test_statement_compound_while_multiline_leading(self):
        i = """while True:

            pass
        """
        o = [t.While(t.Name("True"), [t.Pass()])]
        self.succeed(i, o)


class TestIfStmt(TC):

    rule = "if_stmt"

    def test_if_true_pass(self):
        i = "if True:\n pass\n"
        o = t.If(t.Name("True"), [t.Pass()], [], None)
        self.succeed(i, o)

    def test_if_true_else_pass(self):
        i = "if True:\n pass\nelse:\n pass\n"
        o = t.If(t.Name("True"), [t.Pass()], [], [t.Pass()])
        self.succeed(i, o)

    def test_if_true_elif_else_pass(self):
        i = "if True:\n pass\nelif False:\n pass\nelse:\n pass\n"
        o = t.If(t.Name("True"),
                 [t.Pass()],
                 [(t.Name("False"), [t.Pass()])],
                 [t.Pass()])
        self.succeed(i, o)


class TestWhileStmt(TC):

    rule = "while_stmt"

    def test_while_true_pass(self):
        i = "while True:\n pass\n"
        o = t.While(t.Name("True"), [t.Pass()])
        self.succeed(i, o)

    def test_while_expr_pass(self):
        i = "while 1 + 2:\n pass\n"
        o = t.While(t.Add(t.Num(1), t.Num(2)), [t.Pass()])
        self.succeed(i, o)


class TestFuncdef(TC):

    rule = "funcdef"

    def test_empty_pass(self):
        i = "def empty():\n pass\n"
        o = t.Def("empty", t.Parameters(), [t.Pass()])
        self.succeed(i, o)

    def test_empty_pass_trailing_space(self):
        i = "def empty():\n pass \n"
        o = t.Def("empty", t.Parameters(), [t.Pass()])
        self.succeed(i, o)

    def test_empty_pass_trailing_comment(self):
        i = "def empty():\n pass # comment\n"
        o = t.Def("empty", t.Parameters(), [t.Pass()])
        self.succeed(i, o)

    def test_empty_trailing_comment_pass(self):
        i = "def empty(): # comment\n pass\n"
        o = t.Def("empty", t.Parameters(), [t.Pass()])
        self.succeed(i, o)

    def test_empty_pass_one_line(self):
        i = "def empty(): pass\n"
        o = t.Def("empty", t.Parameters(), [t.Pass()])
        self.succeed(i, o)

    def test_empty_pass_multiline(self):
        i = """def empty():
            pass
            pass
        """
        o = t.Def("empty", t.Parameters(), [t.Pass(), t.Pass()])
        self.succeed(i, o)

    def test_empty_pass_multiline_blanks(self):
        i = """def empty():

            pass
        """
        o = t.Def("empty", t.Parameters(), [t.Pass()])
        self.succeed(i, o)

    def test_empty_pass_multiline_blanks_multiple(self):
        i = """def empty():

            pass

            pass
        """
        o = t.Def("empty", t.Parameters(), [t.Pass(), t.Pass()])
        self.succeed(i, o)

    def test_empty_pass_multiline_blanks_leading(self):
        """
        Yes, there is a trailing space in the middle of this test. It is
        intentional. Please do not touch it.
        """

        i = """def empty():
    
            pass
        """
        o = t.Def("empty", t.Parameters(), [t.Pass()])
        self.succeed(i, o)

    def test_doc_short(self):
        i = """def doc():
            "Short doc."
        """
        o = t.Def("doc", t.Parameters(), [t.Str(None, "Short doc.")])
        self.succeed(i, o)

    def test_doc_long(self):
        i = """def doc():
            '''
            Long doc.
            '''
        """
        o = t.Def("doc", t.Parameters(),
                [t.Str(None, "\n            Long doc.\n            ")])
        self.succeed(i, o)

    def test_add_one_line(self):
        i = "def add(x, y): return x + y\n"
        o = t.Def("add",
                  t.Parameters([t.Name("x"), t.Name("y")]),
                  [t.Return(t.Add(t.Name("x"), t.Name("y")))])
        self.succeed(i, o)

    def test_add_assign(self):
        i = """def add(x, y):
            z = x + y
            return z
        """
        o = t.Def("add",
                  t.Parameters([t.Name("x"), t.Name("y")]),
                  [
                      t.Assign([t.Name("z")], t.Add(t.Name("x"), t.Name("y"))),
                      t.Return(t.Name("z")),
                  ])
        self.succeed(i, o)

    def test_quadratic(self):
        i = """def quadratic(a, b, c):
            d = sqrt(%s)
            a2 = 2 * a
            return (-b + dis) / a2, (-b - dis) / a2
        """ % self.discriminant_string
        o = t.Def("quadratic",
                  t.Parameters([t.Name("a"), t.Name("b"), t.Name("c")]),
                  [
                      t.Assign([t.Name("d")], t.Call(t.Name("sqrt"),
                          t.Arguments([self.discriminant], None, None))),
                      t.Assign([t.Name("a2")], t.Mul(t.Num(2), t.Name("a"))),
                      t.Return(t.Tuple(
                          t.Div(t.Add(t.Negate(t.Name("b")), t.Name("dis")),
                                t.Name("a2")),
                          t.Div(t.Sub(t.Negate(t.Name("b")), t.Name("dis")),
                                t.Name("a2")),
                      )),
                  ])
        self.succeed(i, o)


class TestParameterList(TC):

    rule = "parameter_list"

    def test_params_multiple(self):
        i = "x, y"
        o = t.Parameters([t.Name("x"), t.Name("y")])
        self.succeed(i, o)

    def test_params_args(self):
        i = "*args"
        o = t.Parameters([], t.Name("args"))
        self.succeed(i, o)


class TestObjectDef(TC):

    rule = "objectdef"

    def test_class_depth(self):
        i = "object Depth: depth = 0\n"
        o = t.Object("Depth", [t.Assign([t.Name("depth")], t.Num(0))])
        self.succeed(i, o)

    def test_class_parser(self):
        i = """object Parser:
            pass
        """
        o = t.Object("Parser", [t.Pass()])
        self.succeed(i, o)

    def test_class_parent_kwargs(self):
        i = 'object P: pass\n'
        o = t.Object("P", [t.Pass()])
        self.succeed(i, o)


class TestFileInput(TC):

    rule = "file_input"

    def test_file_input_simple(self):
        i = """
object Simple:
    pass

        """
        o = t.File([t.Object("Simple", [t.Pass()])])
        self.succeed(i, o)

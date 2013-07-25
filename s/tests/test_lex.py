from unittest import TestCase

from s.bytecode import Literal
from s.lex import classify
from s.objects import Bool


class TestClassify(TestCase):

    def test_classify_true(self):
        i = "true"
        o = Literal(Bool(True))
        self.assertEqual(classify(i), o)

from unittest import TestCase

from s.eons import classify, Literal
from s.objects import Bool


class TestClassify(TestCase):

    def test_classify_true(self):
        i = "true"
        o = Literal(Bool(True))
        self.assertEqual(classify(i), o)

from unittest import TestCase

from machine import CAMP

class TestCAMP(TestCase):

    def succeeds(self, bc, input):
        self.assertTrue(CAMP(bc, input).run())

    def fails(self, bc, input):
        self.assertFalse(CAMP(bc, input).run())

    def test_single_char(self):
        self.succeeds("Ex", "x")

    def test_multiple_chars(self):
        self.succeeds("ExEyEz", "xyz")

    def test_trailing(self):
        self.succeeds("Ex", "xy")

    def test_short_empty_string(self):
        self.fails("Ex", "")

    def test_short(self):
        self.fails("ExEy", "x")

    def test_wrong_char(self):
        self.fails("Ex", "y")

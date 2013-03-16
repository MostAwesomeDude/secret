from unittest import TestCase

from machine import CAMP


class TestCAMP(TestCase):

    def succeeds(self, bc, input):
        self.assertTrue(CAMP(bc, input).run())

    def fails(self, bc, input):
        self.assertFalse(CAMP(bc, input).run())


class TestExactly(TestCAMP):

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


class TestOrderedChoice(TestCAMP):
    """
    'x' | 'y' -> "H\x04ExM\x02Ey"
    ('x' | 'y') | 'z' -> "H\x0aH\x04ExM\x02EyM\x02Ez"
    'x' | ('y' | 'z') -> "H\x04ExM\x08H\x04EyM\x02Ez"
    """

    def test_x_or_y_first(self):
        self.succeeds("H\x04ExM\x02Ey", "x")

    def test_x_or_y_second(self):
        self.succeeds("H\x04ExM\x02Ey", "y")

    def test_x_or_y_neither(self):
        self.fails("H\x04ExM\x02Ey", "z")

    def test_x_or_y_or_z_left_first(self):
        self.succeeds("H\x0aH\x04ExM\x02EyM\x02Ez", "x")

    def test_x_or_y_or_z_left_third(self):
        self.succeeds("H\x0aH\x04ExM\x02EyM\x02Ez", "z")

    def test_x_or_y_or_z_right_first(self):
        self.succeeds("H\x04ExM\x08H\x04EyM\x02Ez", "x")

    def test_x_or_y_or_z_right_third(self):
        self.succeeds("H\x04ExM\x08H\x04EyM\x02Ez", "z")

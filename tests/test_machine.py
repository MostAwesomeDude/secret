# Copyright (C) 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.
from unittest import TestCase

from machine import CAMP


class TestCAMP(TestCase):

    def succeeds(self, bc, input):
        self.assertTrue(CAMP(bc, input).run())

    def fails(self, bc, input):
        self.assertFalse(CAMP(bc, input).run())


class TestAnything(TestCAMP):

    def test_anything(self):
        self.succeeds("A", "x")

    def test_anything_eof(self):
        self.fails("A", "")


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


class TestNot(TestCAMP):
    """
    eof => ~anything -> "H\x04AM\x00F"
    """

    def test_eof(self):
        self.succeeds("H\x04AM\x00F", "")

    def test_eof_fail(self):
        self.fails("H\x04AM\x00F", "x")

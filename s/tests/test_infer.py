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

from s.infer import CantUnify, Effect, unify_var


class TestUnifyVar(TestCase):

    def test_unify_anything(self):
        x = "*"
        y = "*"
        result = "*", (None, None)
        self.assertEqual(unify_var(x, y), result)

    def test_unify_concrete_anything(self):
        x = "Test"
        y = "*"
        result = "Test", (None, None)
        self.assertEqual(unify_var(x, y), result)

    def test_unify_concrete_type(self):
        x = "Test"
        y = "a"
        result = "Test", ("a", "Test")
        self.assertEqual(unify_var(x, y), result)

    def test_unify_concrete_fail(self):
        x = "Test"
        y = "Example"
        self.assertRaises(CantUnify, unify_var, x, y)


class TestEffect(TestCase):

    def test_fuse_empty_single(self):
        first = Effect([], [])
        second = Effect([], ["*"])
        result = Effect([], ["*"])
        self.assertEqual(first.fuse(second), result)

    def test_fuse_single_binary(self):
        first = Effect([], ["*"])
        second = Effect(["*", "*"], ["*"])
        result = Effect(["*"], ["*"])
        self.assertEqual(first.fuse(second), result)

    def test_fuse_single_swap(self):
        first = Effect([], ["*"])
        second = Effect(["*", "*"], ["*", "*"])
        result = Effect(["*"], ["*", "*"])
        self.assertEqual(first.fuse(second), result)

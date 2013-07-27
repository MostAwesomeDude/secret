from unittest import TestCase

from s.infer import Effect, unify_var


class TestUnifyVar(TestCase):

    def test_unify_anything(self):
        x = "*"
        y = "*"
        result = "*", None
        self.assertEqual(unify_var(x, y), result)

    def test_unify_concrete_anything(self):
        x = "Test"
        y = "*"
        result = "Test", None
        self.assertEqual(unify_var(x, y), result)

    def test_unify_concrete_type(self):
        x = "Test"
        y = "a"
        result = "Test", ("a", "Test")
        self.assertEqual(unify_var(x, y), result)


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

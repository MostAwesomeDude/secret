from unittest import TestCase

from s.infer import Effect

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

class BadMessage(Exception):
    """
    The message was bad.
    """


class Object(object):
    """
    An object.
    """

    def __str__(self):
        return "<Object>"

    def call(self, message, args):
        raise BadMessage()


class Int(Object):
    """
    An int.
    """

    def __init__(self, i):
        self._i = i

    def __str__(self):
        return str(self._i)

    __repr__ = __str__

    def call(self, message, args):
        if message == "mul":
            assert len(args) == 1
            other = args[0]
            assert isinstance(other, Int)
            return Int(self._i * other._i)
        else:
            raise BadMessage()


class Str(Object):
    """
    A str.
    """

    def __init__(self, s):
        self._s = s

    def __str__(self):
        return repr(self._s)

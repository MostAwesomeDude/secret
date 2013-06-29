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


class Bool(Object):
    """
    A boxed bool.
    """

    def __init__(self, b):
        self._b = b

    def __str__(self):
        if self._b:
            return "True"
        else:
            return "False"


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


class List(Object):
    """
    A boxed list.
    """

    def __init__(self, l):
        self._l = l

    def __str__(self):
        segments = ["["]
        for i in self._l:
            segments.append(str(i))
        segments.append("]")
        return "".join(segments)

    def call(self, message, args):
        if message == "push":
            assert len(args) == 1
            self.push(args[0])
        else:
            raise BadMessage()

    def push(self, item):
        self._l.append(item)


class Str(Object):
    """
    A str.
    """

    def __init__(self, s):
        self._s = s

    def __str__(self):
        return repr(self._s)


class Void(Object):
    """
    A nullary value.
    """

    def __str__(self):
        return "<Void>"


def eq(x, y):
    """
    Unbox two objects, figure out whether they're equal, and return a Bool
    with that value.
    """

    if isinstance(x, Bool) and isinstance(y, Bool):
        rv = x._b == y._b
    elif isinstance(x, Int) and isinstance(y, Int):
        rv = x._i == y._i
    elif isinstance(x, Str) and isinstance(y, Str):
        rv = x._s == y._s
    else:
        rv = x is y

    return Bool(rv)

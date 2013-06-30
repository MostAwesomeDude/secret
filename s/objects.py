class BadMessage(Exception):
    """
    The message was bad.
    """


class Object(object):
    """
    An object.
    """

    def repr(self):
        return "<Object>"

    def call(self, message, args):
        raise BadMessage()


class Bool(Object):
    """
    A boxed bool.
    """

    def __init__(self, b):
        self._b = b

    def repr(self):
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

    def repr(self):
        return str(self._i)

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

    def repr(self):
        segments = ["["]
        for i in self._l:
            segments.append(i.repr())
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

    def repr(self):
        l = []
        for char in self._s:
            if char == '"':
                l.append('\\"')
            elif char == "\\":
                l.append("\\\\")
            else:
                l.append(char)
        return '"%s"' % "".join(l)


class UserObject(Object):
    """
    A user-defined object.
    """

    def __init__(self, methods):
        self._ms = {}

        assert isinstance(methods, List)
        ms = methods._l
        for m in ms:
            assert isinstance(m, List)
            l = m._l
            assert len(l) == 2
            name, code = l
            self._ms[name] = code

    def repr(self):
        return "<UserObject>"


class Void(Object):
    """
    A nullary value.
    """

    def repr(self):
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

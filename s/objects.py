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

    def __eq__(self, other):
        return False

    def call(self, message, args):
        raise BadMessage()


class Bool(Object):
    """
    A boxed bool.
    """

    def __init__(self, b):
        self._b = b

    def __eq__(self, other):
        if not isinstance(other, Bool):
            return False
        return self._b == other._b

    def repr(self):
        if self._b:
            return "True"
        else:
            return "False"


class Ejecting(Exception):
    """
    The stack is being unwound by an ejector.
    """

    def __init__(self, ejector, value):
        self._ejector = ejector
        self._value = value


class Ejector(Object):
    """
    An ejector.

    Ejectors can be fired once, and only once, to perform a non-local exit.
    """

    _fired = False

    def __init__(self, stack):
        self._stack = stack

    def __enter__(self):
        pass

    def __exit__(self, exc_type, exc_value, traceback):
        # Rationale is as follows.
        # Even if the exception is *not* triggering this ejector, it's
        # certainly unwinding the stack past where this ejector was created.
        # As a result, the ejector must surely be invalidated at this point.
        self._fired = True

        if isinstance(exc_value, Ejecting):
            if exc_value._ejector is self:
                # Seems legit.
                self._stack.push(exc_value._value)
                # Return True to stop the exception from propagating.
                return True

        # Resume exception handling.
        return False

    def eject(self, value):
        if self._fired:
            raise Exception("Fired already-fired ejector!")

        raise Ejecting(self, value)

    def repr(self):
        return "Ejector(%s)" % ("fired" if self._fired else "fresh")


class Int(Object):
    """
    An int.
    """

    def __init__(self, i):
        self._i = i

    def __eq__(self, other):
        if not isinstance(other, Int):
            return False
        return self._i == other._i

    def repr(self):
        return str(self._i)

    def call(self, message, args):
        if message == "add":
            assert len(args) == 1
            other = args[0]
            assert isinstance(other, Int)
            return Int(self._i + other._i)
        elif message == "eq":
            assert len(args) == 1
            other = args[0]
            assert isinstance(other, Int)
            return Bool(self._i == other._i)
        elif message == "mul":
            assert len(args) == 1
            other = args[0]
            assert isinstance(other, Int)
            return Int(self._i * other._i)
        elif message == "sub":
            assert len(args) == 1
            other = args[0]
            assert isinstance(other, Int)
            return Int(self._i - other._i)
        else:
            raise BadMessage()


class List(Object):
    """
    A boxed list.
    """

    def __init__(self, l):
        self._l = l

    def __eq__(self, other):
        if not isinstance(other, List):
            return False
        return self._l == other._l

    def repr(self):
        segments = []
        for i in self._l:
            segments.append(i.repr())
        return "[%s]" % ", ".join(segments)

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

    def __eq__(self, other):
        if not isinstance(other, Str):
            return False
        return self._s == other._s

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


class Promise(Object):
    """
    A deferred call which eventually resolves into either a reference to an
    object or a broken reference.
    """

    _value = None

    def __init__(self, target, message, args):
        self._target = target
        self._message = message
        self._args = args

    def repr(self):
        if self.resolved():
            return "Promise(%s)" % self._value.repr()
        else:
            return "Promise(%s, %s, %s)" % (self._target.repr(),
                    self._message.repr(), self._args.repr())

    def resolved(self):
        return self._value is not None

    def resolve(self, value):
        self._value = value


class UserObject(Object):
    """
    A user-defined object.
    """

    def __init__(self, vm, methods):
        self._vm = vm
        self._ms = {}

        assert isinstance(methods, List)
        ms = methods._l
        for m in ms:
            assert isinstance(m, List)
            l = m._l
            assert len(l) == 2
            name, code = l
            assert isinstance(name, Str)
            assert isinstance(code, Str)
            self._ms[name._s] = code._s

    def repr(self):
        return "<UserObject>"

    def call(self, message, args):
        method = self._ms[message]
        vm = self._vm
        return vm.run_method(self, method, args)


class Void(Object):
    """
    A nullary value.
    """

    def repr(self):
        return "<Void>"


def name_for_object(obj):
    """
    Given an object, return the name for that object.
    """

    if isinstance(obj, Bool):
        return "Bool"
    elif isinstance(obj, Int):
        return "Int"
    elif isinstance(obj, List):
        return "List"
    elif isinstance(obj, Promise):
        return "Promise"
    elif isinstance(obj, Str):
        return "Str"
    elif isinstance(obj, UserObject):
        return "UserObject"
    elif isinstance(obj, Void):
        return "Void"
    else:
        return "*"


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

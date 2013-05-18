import sys


DROP = 0
DUP = 1
SWAP = 2

PUSH_CONST = 3
LOAD = 4
CALL = 5
SEND = 6
GET = 7
SET = 8
NEW = 9

EQ = 10
MAKE_LIST = 11
SHOW = 12


class Object(object):
    """
    A black-box generic object.
    """

    def __init__(self):
        pass

    def call(self, message, arguments):
        assert isinstance(message, Str)
        m = message._s
        if m == "__str__":
            return Str(self.__str__())
        return NoneObject()

    def __str__(self):
        return "<Object>"


class NoneObject(Object):
    """
    A nullary value.
    """

    def __str__(self):
        return "<None>"


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


class Str(Object):
    """
    A boxed str.
    """

    _s = None

    def __init__(self, s):
        self._s = s

    def __str__(self):
        return "\"" + self._s + "\""


class UserObject(Object):
    """
    User-defined object.
    """

    def __init__(self, slots, messages):
        self._slots = slots
        self._messages = messages

    def get_slot(self, slot):
        return self._slots[slot]

    def set_slot(self, slot, value):
        self._slots[slot] = value

    def call(self, message, args):
        # XXX hmmmm....
        return None


class Int(Object):
    """
    A boxed int.
    """

    def __init__(self, i):
        self._i = i

    def call(self, message, arguments):
        assert isinstance(message, Str)
        m = message._s

        if m == "__mul__":
            assert isinstance(arguments, List)
            assert len(arguments._l) == 1
            arg = arguments._l[0]
            assert isinstance(arg, Int)
            return Int(self._i * arg._i)

        return Object.call(self, message, arguments)

    def __str__(self):
        return str(self._i)


class List(Object):
    """
    A boxed list.
    """

    def __init__(self, l):
        self._l = l

    def __str__(self):
        segments = ["["]
        for i in self._l:
            segments.append(i.__str__())
        segments.append("]")
        return "".join(segments)


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


class Stack(object):
    """
    A simple stack with some convenience methods.
    """

    def __init__(self):
        self._storage = []

    def drop(self):
        self._storage.pop()

    def pop(self):
        return self._storage.pop()

    def push(self, x):
        self._storage.append(x)

    def peek(self):
        if len(self._storage):
            return self._storage[-1]
        else:
            return Object()


def eval(bc, frame, scope):
    stack = Stack()
    pc = 0

    while pc < len(bc):
        inst = bc[pc]

        print "pc: %d inst: %d" % (pc, inst)
        print "depth: %d tos: %s" % (len(stack._storage), stack.peek().__str__())

        if inst == DROP:
            stack.drop()
        elif inst == DUP:
            stack.push(stack.peek())
        elif inst == SWAP:
            x = stack.pop()
            y = stack.pop()
            stack.push(x)
            stack.push(y)
        elif inst == PUSH_CONST:
            pc += 1
            index = bc[pc]
            stack.push(frame[index])
        elif inst == LOAD:
            pass # XXX
        elif inst == CALL:
            args = stack.pop()
            message = stack.pop()
            target = stack.pop()
            rv = target.call(message, args)
            stack.push(rv)
        elif inst == SEND:
            pass # XXX
        elif inst == GET:
            pass # XXX
        elif inst == SET:
            pass # XXX
        elif inst == NEW:
            pass # XXX
        elif inst == EQ:
            x = stack.pop()
            y = stack.pop()
            rv = eq(x, y)
            stack.push(rv)
        elif inst == MAKE_LIST:
            pc += 1
            length = bc[pc]
            l = []
            i = 0
            while i < length:
                l.append(stack.pop())
                i += 1
            stack.push(List(l))
        elif inst == SHOW:
            print stack.peek().__str__()

        pc += 1


def run(bc):
    frame = [Bool(True), Int(6), Int(7), Str("__mul__")]
    scope = {}
    return eval(bc, frame, scope)


def entry_point(argv):
    run([
        # Push True.
        PUSH_CONST, 0,
        # Push True.
        PUSH_CONST, 0,
        # Are they equal?
        EQ,
        # Push 6.
        PUSH_CONST, 1,
        # Push 7.
        PUSH_CONST, 2,
        # 7 -> [7].
        MAKE_LIST, 1,
        # Push "__mul__".
        PUSH_CONST, 3,
        # (6).__mul__([7]).
        SWAP, CALL,
        # Show.
        SHOW,
    ])
    return 0


def target(*args):
    return entry_point, None


if __name__ == "__main__":
    entry_point(sys.argv)
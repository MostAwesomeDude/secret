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

MAKE_LIST = 10
SHOW = 11


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


class Str(Object):
    """
    A boxed str.
    """

    _s = None

    def __init__(self, s):
        self._s = s

    def __str__(self):
        return self._s


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
        return str(self._l)


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
            print "call: args: %s message: %s target: %s" % (str(args),
                    str(message), str(target))
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
    frame = [Int(6), Int(7), Str("__mul__")]
    scope = {}
    return eval(bc, frame, scope)


def entry_point(argv):
    run([
        # Push 6.
        PUSH_CONST, 0,
        # Push 7.
        PUSH_CONST, 1,
        # 7 -> [7].
        MAKE_LIST, 1,
        # Push "__mul__".
        PUSH_CONST, 2,
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

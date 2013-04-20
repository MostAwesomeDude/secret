import sys


SHOW = -1

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
        pass

    def __str__(self):
        return str(self._i)


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
        print "tos: %s" % str(stack.peek())

        if inst == SHOW:
            print stack.peek().__str__()
        elif inst == DROP:
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

        pc += 1


def run(bc):
    frame = [Int(42)]
    scope = {}
    return eval(bc, frame, scope)


def entry_point(argv):
    run([PUSH_CONST, 0, SHOW])
    return 0


def target(*args):
    return entry_point, None


if __name__ == "__main__":
    entry_point(sys.argv)

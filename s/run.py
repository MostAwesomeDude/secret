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
        return self._storage[-1]


class Object(object):
    """
    Main definition of objects.
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


def eval(bc, frame, scope):
    stack = Stack()
    pc = 0

    while pc < len(bc):
        inst = bc[pc]

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
            # XXX can't keep scope on the stack due to polymorphic
            # limitations...
            name = stack.pop()
            stack.pop()
            stack.push(scope[name])
        elif inst == CALL:
            args = stack.pop()
            message = stack.pop()
            target = stack.pop()
            rv = target.call(message, args)
            stack.push(rv)
        elif inst == SEND:
            pass # XXX
        elif inst == GET:
            slot = stack.pop()
            target = stack.pop()
            rv = target.get_slot(slot)
            stack.push(rv)
        elif inst == SET:
            value = stack.pop()
            slot = stack.pop()
            target = stack.pop()
            target.set_slot(slot, value)
        elif inst == NEW:
            slots = stack.pop()
            messages = stack.pop()
            obj = Object(slots, messages)
            stack.push(obj)

    pc += 1


def run(bc):
    frame = []
    scope = {}
    return eval(bc, frame, scope)


def entry_point(argv):
    return 0


def target(*args):
    return entry_point, None


if __name__ == "__main__":
    entry_point(sys.argv)

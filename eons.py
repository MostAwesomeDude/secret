import os
import sys


builtins = {
    "()":     (0, 1),
    ".":      (3, 1),
    "<-":     (3, 1),
    "<meth":  (2, 1),
    "drop":   (1, 0),
    "dup":    (1, 2),
    "object": (1, 1),
    "print":  (1, 0),
    "<arg":   (2, 1),
    "swap":   (2, 2),
}


def combine_stack_effects(fi, fo, si, so):
    if fo > si:
        so += fo - si
    elif si > fo:
        fi += si - fo
    return fi, so


def infer_stack_effect(tokens):
    i = o = 0

    for token in tokens:
        if token in builtins:
            ni, no = builtins[token]
            i, o = combine_stack_effects(i, o, ni, no)
        else:
            o += 1

    return i, o


class StackUnderflow(Exception):
    """
    The stack underflowed.
    """


class BadMessage(Exception):
    """
    The message was bad.
    """


class Stack(object):
    """
    A simple stack with some convenience methods.
    """

    def __init__(self):
        self._storage = []

    def drop(self):
        self._storage.pop()

    def pop(self):
        if len(self._storage):
            return self._storage.pop()
        else:
            raise StackUnderflow()

    def push(self, x):
        self._storage.append(x)

    def peek(self):
        if len(self._storage):
            return self._storage[-1]
        else:
            raise StackUnderflow()


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
        return self._s


class Machine(object):
    """
    A virtual machine which executes Eons.
    """

    def __init__(self):
        self.stack = Stack()

    def pass_message(self, target, message, args):
        assert isinstance(message, Str)
        print "~ Passing to %r: %s, %r" % (target, message, args)
        return target.call(message._s, args)

    def execute(self, token, context):
        stack = self.stack

        print "Executing", token

        if False:
            pass
        elif token == "()":
            stack.push([])
        elif token == ".":
            args = stack.pop()
            name = stack.pop()
            target = stack.pop()
            result = self.pass_message(target, name, args)
            stack.push(result)
        elif token == "<-":
            args = stack.pop()
            name = stack.pop()
            target = stack.pop()
            # XXX wrong
            stack.push((target, name, args))
        elif token == "<meth":
            name = stack.pop()
            code = stack.pop()
            stack.push((name, code))
        elif token == "drop":
            stack.pop()
        elif token == "dup":
            stack.push(stack.peek())
        elif token == "object":
            methods = stack.pop()
            d = dict(methods)
            stack.push(d)
        elif token == "<arg":
            obj = stack.pop()
            l = stack.peek()
            l.append(obj)
        elif token == "print":
            print stack.pop()
        elif token == "swap":
            x = stack.pop()
            y = stack.pop()
            stack.push(x)
            stack.push(y)
        else:
            stack.push(token)

    def run_phrase(self, name, phrases):
        phrase = phrases[name]
        for word in phrase:
            self.execute(word, phrases)


def parse_pieces(data):
    lines = data.split("\n")
    pieces = []
    for line in lines:
        pieces.extend(line.split(" "))
    filtered = [piece for piece in pieces if piece]

    def try_int(x):
        try:
            return Int(int(x))
        except:
            return x

    def maybe_str(x):
        try:
            if x.startswith("\"") and x.endswith("\""):
                return Str(x[1:-1])
            else:
                return x
        except:
            return x

    with_ints = [try_int(x) for x in filtered]
    return [maybe_str(x) for x in with_ints]


def parse_phrases(pieces):
    phrases = {}

    while pieces:
        try:
            start = pieces.index(":")
            end = pieces.index(";")
        except ValueError:
            break

        name = pieces[start + 1]
        phrase = pieces[start + 2:end]
        phrases[name] = phrase

        pieces = pieces[end + 1:]

    return phrases


def read_file(name):
    fd = os.open(name, os.O_RDONLY, 0777)
    l = []
    l.append(os.read(fd, 4096))
    while l[-1]:
        l.append(os.read(fd, 4096))
    return "".join(l)


def entry_point(argv):
    data = read_file(argv[1])
    pieces = parse_pieces(data)
    phrases = parse_phrases(pieces)

    for word, phrase in phrases.items():
        print "Word:", word
        print "Tokens:", " ".join(map(str, phrase))
        print "Stack effect:", infer_stack_effect(phrase)

    if "main" in phrases:
        vm = Machine()
        vm.run_phrase("main", phrases)

    return 0


def target(*args):
    return entry_point, None


if __name__ == "__main__":
    entry_point(sys.argv)

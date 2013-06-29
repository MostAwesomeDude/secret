import os
import sys


# Bytecode numbering.
(
    DROP, DUP, SWAP,
    ARGS, TO_ARG,
    MAKE_METHOD,
    OBJECT, TO_OBJECT,
    CALL, SEND,
    PRINT,
) = range(11)


bytecodes = {
    "()":     ARGS,
    ".":      CALL,
    "<-":     SEND,
    "<arg":   TO_ARG,
    "<meth":  MAKE_METHOD,
    "drop":   DROP,
    "dup":    DUP,
    "object": OBJECT,
    "print":  PRINT,
    "swap":   SWAP,
}

bytecode_names = dict([(bytecodes[k], k) for k in bytecodes])


builtins = {
    ARGS:        (0, 1),
    CALL:        (3, 1),
    DROP:        (1, 0),
    DUP:         (1, 2),
    MAKE_METHOD: (2, 1),
    OBJECT:      (1, 1),
    PRINT:       (1, 0),
    SEND:        (3, 1),
    SWAP:        (2, 2),
    TO_ARG:      (2, 1),
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


class Bytecode(object):
    """
    A bytecode for Eons.
    """

class Literal(Bytecode):
    """
    A literal in bytecode.
    """

    def __init__(self, l):
        self._l = l

    def __str__(self):
        return "Literal(%s)" % self._l


class Instruction(Bytecode):
    """
    A bytecode instruction.
    """

    def __init__(self, i):
        self._i = i

    def __str__(self):
        return "Instruction(%r)" % bytecode_names[self._i]


class Reference(Bytecode):
    """
    A reference to a phrase.
    """

    def __init__(self, r):
        self._r = r

    def __str__(self):
        return "Reference(%r)" % self._r


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

        if isinstance(token, Literal):
            stack.push(token._l)
        else:
            assert isinstance(token, Instruction)
            i = token._i

            if False:
                pass
            elif i == ARGS:
                # XXX wrong type
                stack.push([])
            elif i == CALL:
                args = stack.pop()
                name = stack.pop()
                target = stack.pop()
                result = self.pass_message(target, name, args)
                stack.push(result)
            elif i == SEND:
                args = stack.pop()
                name = stack.pop()
                target = stack.pop()
                # XXX wrong type
                stack.push((target, name, args))
            elif i == MAKE_METHOD:
                name = stack.pop()
                code = stack.pop()
                # XXX wrong type
                stack.push((name, code))
            elif i == DROP:
                stack.pop()
            elif i == DUP:
                stack.push(stack.peek())
            elif i == OBJECT:
                methods = stack.pop()
                d = dict(methods)
                # XXX wrong type
                stack.push(d)
            elif i == TO_ARG:
                obj = stack.pop()
                # XXX wrong type
                l = stack.peek()
                l.append(obj)
            elif i == PRINT:
                print stack.pop()
            elif i == SWAP:
                x = stack.pop()
                y = stack.pop()
                stack.push(x)
                stack.push(y)
            else:
                print "Unknown instruction", i

    def run_phrase(self, name, phrases):
        phrase = phrases[name]
        for word in phrase:
            self.execute(word, phrases)


def classify(x):
    if x in bytecodes:
        return Instruction(bytecodes[x])
    else:
        try:
            return Literal(Int(int(x)))
        except ValueError:
            pass

        if x.startswith("\"") and x.endswith("\""):
            return Literal(Str(x[1:-1]))

    return Reference(x)


def parse_pieces(data):
    lines = data.split("\n")
    pieces = []
    for line in lines:
        pieces.extend(line.split(" "))
    filtered = [piece for piece in pieces if piece]

    return filtered


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


def classify_phrases(phrases):
    d = {}
    for word in phrases.keys():
        d[word] = [classify(w) for w in phrases[word]]
    return d


def entry_point(argv):
    data = read_file(argv[1])
    pieces = parse_pieces(data)
    phrases = classify_phrases(parse_phrases(pieces))

    for word, phrase in phrases.items():
        print "Word:", word
        print "Tokens:", " ".join([str(w) for w in phrase])
        print "Stack effect:", infer_stack_effect(phrase)

    if "main" in phrases:
        vm = Machine()
        vm.run_phrase("main", phrases)

    return 0


def target(*args):
    return entry_point, None


if __name__ == "__main__":
    entry_point(sys.argv)

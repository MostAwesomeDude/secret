import os
import sys

from s.objects import Bool, Int, List, Promise, Str, UserObject


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
        if isinstance(token, Instruction):
            ni, no = builtins[token._i]
            i, o = combine_stack_effects(i, o, ni, no)
        else:
            o += 1

    return i, o


class StackUnderflow(Exception):
    """
    The stack underflowed.
    """


class Stack(object):
    """
    A simple stack with some convenience methods.
    """

    def __init__(self):
        self._storage = []

    def repr(self):
        return "Stack(%s)" % ", ".join([x.repr() for x in self._storage])

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


class Bytecode(object):
    """
    A bytecode for Eons.
    """

    def repr(self):
        return "<Blank Bytecode>"


class Literal(Bytecode):
    """
    A literal in bytecode.
    """

    def __init__(self, l):
        self._l = l

    def __eq__(self, other):
        if not isinstance(other, Literal):
            return False
        return self._l == other._l

    def repr(self):
        return "Literal(%s)" % self._l.repr()


class Instruction(Bytecode):
    """
    A bytecode instruction.
    """

    def __init__(self, i):
        self._i = i

    def repr(self):
        return "Instruction('%s')" % bytecode_names[self._i]


class Reference(Bytecode):
    """
    A reference to a phrase.
    """

    def __init__(self, r):
        self._r = r

    def repr(self):
        return "Reference(%s)" % self._r


class Word(Bytecode):
    """
    A phrase being called directly as a word.
    """

    def __init__(self, w):
        self._w = w

    def repr(self):
        return "Word(%s)" % self._w


class Machine(object):
    """
    A virtual machine which executes Eons.
    """

    def __init__(self, phrases):
        self.phrases = phrases

        self.stack = Stack()
        self.promises = []

    def pass_message(self, target, message, args):
        assert isinstance(message, Str)
        assert isinstance(args, List)
        print "~ Passing to %s: %s, %s" % (target.repr(), message.repr(),
                args.repr())
        return target.call(message._s, args._l)

    def execute(self, token):
        stack = self.stack

        print self.stack.repr()
        print "Executing", token.repr()

        if isinstance(token, Literal):
            stack.push(token._l)
        elif isinstance(token, Reference):
            # XXX Probably not the best type for this.
            stack.push(Str(token._r))
        elif isinstance(token, Word):
            self.run_phrase(token._w)
        elif isinstance(token, Instruction):
            i = token._i

            if False:
                pass
            elif i == ARGS:
                stack.push(List([]))
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

                promise = Promise(target, name, args)
                self.promises.append(promise)

                stack.push(promise)
            elif i == MAKE_METHOD:
                name = stack.pop()
                code = stack.pop()
                stack.push(List([name, code]))
            elif i == DROP:
                stack.pop()
            elif i == DUP:
                stack.push(stack.peek())
            elif i == OBJECT:
                methods = stack.pop()

                obj = UserObject(self, methods)

                stack.push(obj)
            elif i == TO_ARG:
                obj = stack.pop()
                l = stack.peek()
                # Check the type. Don't use an assertion, as it will convince
                # RPython that the entire stack should be Lists.
                if isinstance(l, List):
                    l.push(obj)
                else:
                    raise TypeError("Couldn't push into non-List %s" % l)
            elif i == PRINT:
                print stack.pop().repr()
            elif i == SWAP:
                x = stack.pop()
                y = stack.pop()
                stack.push(x)
                stack.push(y)
            else:
                print "Unknown instruction", i
        else:
            print "Can't handle", token

    def run_promise(self, promise):
        print "Running promise", promise.repr()
        result = self.pass_message(promise._target, promise._message,
                                   promise._args)
        print "Result is", result.repr()
        promise.resolve(result)
        # XXX run callbacks here
        print promise.repr()

    def run_promises(self):
        while len(self.promises):
            promises, self.promises = self.promises, []
            for promise in promises:
                # Pass in the original codebase that spawned the promise.
                self.run_promise(promise)

    def run_method(self, obj, method, args):
        self.stack.push(obj)
        for arg in args:
            self.stack.push(arg)

        self.run_phrase(method)

        return self.stack.pop()

    def run_phrase(self, name):
        phrase = self.phrases[name]
        for word in phrase:
            self.execute(word)

    def run(self, name):
        # First, run the initial phrase that we were asked to run.
        self.run_phrase(name)

        # Then, take subsequent turns for each promise that we were asked to
        # make.
        self.run_promises()


def classify(x):
    if x in bytecodes:
        return Instruction(bytecodes[x])
    else:
        try:
            return Literal(Int(int(x)))
        except ValueError:
            pass

        if x.startswith("\"") and x.endswith("\""):
            if len(x) > 2:
                # RPython has trouble with this bit of math.
                end = len(x) - 1
                assert end > 1
                return Literal(Str(x[1:end]))
            else:
                # Empty string: "" or "
                return Literal(Str(""))

        if x == "true":
            return Literal(Bool(True))
        if x == "false":
            return Literal(Bool(False))

        if x.startswith("*") and len(x) > 1:
            return Reference(x[1:])

    return Word(x)


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
    if len(argv) < 2:
        print "Not given any file to run..."
        return 1

    data = read_file(argv[1])
    pieces = parse_pieces(data)
    phrases = classify_phrases(parse_phrases(pieces))

    for word, phrase in phrases.items():
        print "Word:", word
        print "Tokens:", " ".join([w.repr() for w in phrase])
        print "Stack effect:", infer_stack_effect(phrase)

    if "main" in phrases:
        vm = Machine(phrases)
        vm.run("main")

    return 0


def target(*args):
    return entry_point, None


if __name__ == "__main__":
    entry_point(sys.argv)

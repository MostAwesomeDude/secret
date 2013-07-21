# Bytecode numbering.
(
    DROP, DUP, SWAP,
    ARGS, TO_ARG,
    MAKE_METHOD,
    OBJECT,
    CALL, SEND,
    PRINT,
) = range(10)


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

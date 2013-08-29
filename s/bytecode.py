# Bytecode numbering.
(
    NEWE,
    DROP, DUP, OVER, SWAP, ROT,
    ARGS, TO_ARG,
    MAKE_METHOD,
    OBJECT,
    CALL, SEND,
    ESCAPE, EJECT,
    IF,
    PRINT, STACK,
) = range(17)


bytecodes = {
    "()":     ARGS,
    ".":      CALL,
    "<-":     SEND,
    "<arg":   TO_ARG,
    "<meth":  MAKE_METHOD,
    "drop":   DROP,
    "dup":    DUP,
    "e":      NEWE,
    "eject":  EJECT,
    "escape": ESCAPE,
    "if":     IF,
    "object": OBJECT,
    "over":   OVER,
    "print":  PRINT,
    "rot":    ROT,
    "stack":  STACK,
    "swap":   SWAP,
}

bytecode_names = dict([(bytecodes[k], k) for k in bytecodes])


builtins = {
    ARGS:        ([], ["List"]),
    CALL:        (["*", "Str", "List"], ["*"]),
    DROP:        (["*"], []),
    DUP:         (["a"], ["a", "a"]),
    EJECT:       (["Ejector", "*"], ["*"]),
    ESCAPE:      (["*"], ["*"]),
    IF:          (["Bool", "a", "a"], ["*"]),
    MAKE_METHOD: (["*", "Str"], ["List"]),
    NEWE:        ([], ["E"]),
    OBJECT:      (["List"], ["UserObject"]),
    OVER:        (["a", "b"], ["a", "b", "a"]),
    PRINT:       (["*"], []),
    ROT:         (["a", "b", "c"], ["b", "c", "a"]),
    SEND:        (["*", "Str", "List"], ["Promise"]),
    STACK:       ([], []),
    SWAP:        (["a", "b"], ["b", "a"]),
    TO_ARG:      (["List", "*"], ["List"]),
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

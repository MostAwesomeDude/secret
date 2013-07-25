from s.bytecode import builtins, Instruction, Literal, Reference, Word


class Effect(object):
    def __init__(self, i, o):
        self.i = i
        self.o = o

    def repr(self):
        return "Effect(%s, %s)" % (str(self.i), str(self.o))

    def diff(self):
        return len(self.o) - len(self.i)

    def promote(self, depth):
        l = ["*"] * depth
        i = l + self.i
        o = l + self.o
        return Effect(i, o)

    def unify(self, other):
        # XXX
        if self.diff() != other.diff():
            pass

    def fuse(self, other):
        d = len(other.i) - len(self.o)
        if d > 0:
            self = self.promote(d)
        elif d < 0:
            other = other.promote(-d)
        i = self.i
        o = other.o
        return Effect(i, o)


def infer_stack_effect(tokens, library):
    current = Effect([], [])

    for token in tokens:
        if isinstance(token, (Literal, Reference)):
            effect = Effect([], ["*"])
        elif isinstance(token, Instruction):
            i, o = builtins[token._i]
            effect = Effect(["*"] * i, ["*"] * o)
        elif isinstance(token, Word):
            effect = library[token._w]

        current = current.fuse(effect)

    return effect

def infer_phrases(phrases):
    inferred = {}
    to_infer = phrases.items()

    # Ugh, what a cruddy algorithm. Ideally, we'd do this with a topological
    # sort.
    while to_infer:
        word, phrase = to_infer.pop()
        try:
            effect = infer_stack_effect(phrase, inferred)
            inferred[word] = effect
            print "Word:", word
            print "Tokens:", " ".join([w.repr() for w in phrase])
            print "Stack effect:", effect.repr()
        except KeyError:
            to_infer.insert(0, (word, phrase))

    return inferred

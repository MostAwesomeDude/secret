from s.bytecode import builtins, Instruction, Literal, Reference, Word


class CantUnify(Exception):
    """
    Something couldn't be unified with something else.
    """


def unify_var(x, y):
    if x == "*":
        return y, None
    elif y == "*":
        return x, None
    elif not x.islower() and y.islower():
        return x, (y, x)
    elif x.islower() and not y.islower():
        return y, (x, y)
    elif x == y:
        return x, None
    else:
        raise CantUnify("Can't unify %s and %s" % (x, y))


def substitute(d, xs):
    return [d[i] if i in d else i for i in xs]


def unify_list(first, second):
    if len(first) != len(second):
        raise CantUnify("Lengths of %s and %s differ" % (first, second))
    l = []
    d = {}
    for x, y in zip(first, second):
        result, sub = unify_var(x, y)
        if sub is not None:
            d[sub[0]] = sub[1]
        l.append(result)
    return l, d


class Effect(object):
    def __init__(self, i, o):
        self.i = i
        self.o = o

    def __eq__(self, other):
        assert isinstance(other, Effect)
        return self.i == other.i and self.o == other.o

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
        if self.diff() != other.diff():
            raise Exception("Couldn't unify")
        d = len(other.i) - len(self.i)
        if d > 0:
            self = self.promote(d)
        elif d < 0:
            other = other.promote(-d)
        i, _ = unify_list(self.i, other.i)
        o, _ = unify_list(self.o, other.o)
        return Effect(i, o)

    def fuse(self, other):
        d = len(other.i) - len(self.o)
        if d > 0:
            self = self.promote(d)
        elif d < 0:
            other = other.promote(-d)
        unified, names = unify_list(self.o, other.i)
        i = substitute(names, self.i)
        o = substitute(names, other.o)
        return Effect(i, o)


def infer_stack_effect(tokens, library):
    current = Effect([], [])

    for token in tokens:
        if isinstance(token, Literal) or isinstance(token, Reference):
            effect = Effect([], ["*"])
        elif isinstance(token, Instruction):
            i, o = builtins[token._i]
            effect = Effect(i, o)
        elif isinstance(token, Word):
            if token._w not in library:
                return None
            effect = library[token._w]
        else:
            continue

        current = current.fuse(effect)

    return current


def dependent_words(phrase):
    l = []
    for token in phrase:
        if isinstance(token, Word):
            l.append(token._w)
    return l


def topological_sort(phrases):
    # This could be so much better with sets.
    l = []
    s = [w for w in phrases if not dependent_words(phrases[w])]
    ws = [w for w in phrases if w not in s]

    while s:
        ws = [w for w in ws if w not in s]
        word = s.pop()
        l.append(word)
        for dependent in ws:
            parents = dependent_words(phrases[dependent])
            if len(parents) == 1 and parents[0] == word:
                s.insert(0, dependent)

    if ws:
        raise Exception("Cyclic words can't be handled yet!")

    return l


def infer_phrases(phrases):
    inferred = {}
    to_infer = phrases.items()

    to_infer = topological_sort(phrases)
    for word in to_infer:
        phrase = phrases[word]
        effect = infer_stack_effect(phrase, inferred)
        if effect is None:
            raise Exception("Inconceivable!")

        inferred[word] = effect
        print "Word:", word
        print "Tokens:", " ".join([w.repr() for w in phrase])
        print "Stack effect:", effect.repr()

    return inferred

# Copyright (C) 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.
from s.bytecode import builtins, Instruction, Literal, Reference, Word
from s.utils import all


class CantUnify(Exception):
    """
    Something couldn't be unified with something else.
    """


def unify_var(x, y):
    if x == "*":
        return y, (None, None)
    elif y == "*":
        return x, (None, None)
    elif not x[0].islower() and y[0].islower():
        return x, (y, x)
    elif x[0].islower() and not y[0].islower():
        return y, (x, y)
    elif x == y:
        return x, (None, None)
    else:
        raise CantUnify("Can't unify %s and %s" % (x, y))


def substitute(d, xs):
    return [d[i] if i in d else i for i in xs]


def unify_list(first, second):
    if len(first) != len(second):
        raise CantUnify("Lengths of %s and %s differ" % (first, second))
    l = []
    d = {}
    # Aw man, there's not a zip()? :c
    for i in range(len(first)):
        x = first[i]
        y = second[i]
        result, sub = unify_var(x, y)
        if sub[0] is not None:
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
            raise CantUnify("%s and %s are too different" % (self.repr(),
                other.repr()))
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
        if isinstance(token, Reference):
            effect = Effect([], ["*"])
        elif isinstance(token, Literal):
            effect = Effect([], [token._l.name])
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
            if all([parent in l for parent in parents]):
                s.append(dependent)

    if ws:
        raise Exception("Phrases are interdependent: %s" % ", ".join(ws))

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

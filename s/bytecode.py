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
# Bytecode numbering.
(
    NEWE,
    DROP, DUP, OVER, SWAP, ROT,
    LIST, APPEND,
    MAKE_METHOD,
    OBJECT,
    CALL, SEND,
    ESCAPE, EJECT,
    IF,
    PRINT, STACK,
) = range(17)


bytecodes = {
    "[]":     LIST,
    ".":      CALL,
    "<-":     SEND,
    "append": APPEND,
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
    APPEND:      (["List", "*"], ["List"]),
    CALL:        (["*", "Str", "List"], ["*"]),
    DROP:        (["*"], []),
    DUP:         (["a"], ["a", "a"]),
    EJECT:       (["Ejector", "*"], ["*"]),
    ESCAPE:      (["*"], ["*"]),
    IF:          (["Bool", "a", "a"], ["*"]),
    LIST:        ([], ["List"]),
    MAKE_METHOD: (["*", "Str"], ["List"]),
    NEWE:        ([], ["E"]),
    OBJECT:      (["List"], ["UserObject"]),
    OVER:        (["a", "b"], ["a", "b", "a"]),
    PRINT:       (["*"], []),
    ROT:         (["a", "b", "c"], ["b", "c", "a"]),
    SEND:        (["*", "Str", "List"], ["Promise"]),
    STACK:       ([], []),
    SWAP:        (["a", "b"], ["b", "a"]),
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

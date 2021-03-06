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
from struct import unpack

fail = object()

def signed(c):
    """
    Interpret a character as a signed byte.
    """

    return unpack("b", c)[0]


class Success(Exception):
    """
    Our parse was successful.
    """


class Failure(Exception):
    """
    Our parse failed.
    """


class CAMP(object):
    """
    Corbin's Abstract Machine for PEGs.

    Bytecode understood by this machine:
        A: Anything
        Ex: Exactly x
        Ji: Jump i
        Hi: cHoice i
        Li: caLl i
        R: Return
        Mi: coMmit i
        F: Fail

    x is a char, and i is a signed offset.
    """

    noisy = True

    pc = 0
    pos = 0

    def __init__(self, bytecode, input):
        self.bytecode = bytecode
        self.stack = []
        self.caps = []

        self.input = input

    def log(self, message):
        if self.noisy:
            print message

    def read(self):
        try:
            bc = self.bytecode[self.pc]
        except IndexError:
            raise Success()
        self.pc += 1
        return bc

    def next(self):
        if self.pc is fail:
            self.log("Backtracking...")

            if not self.stack:
                raise Failure()

            try:
                p, i, c = self.stack.pop()
                self.pc = p
                self.pos = i
                self.caps = c
            except (TypeError, ValueError):
                pass
            finally:
                return

        bc = self.read()

        self.log("pc=%d bc=%s pos=%d stack=%r" %
                (self.pc, bc, self.pos, self.stack))

        if bc == "A":
            if self.pos >= len(self.input):
                self.pc = fail
            else:
                self.pos += 1
        elif bc == "E":
            target = self.read()
            if self.pos >= len(self.input):
                self.pc = fail
            elif self.input[self.pos] == target:
                self.pos += 1
            else:
                self.pc = fail
        elif bc == "J":
            target = signed(self.read())
            self.pc += target
        elif bc == "H":
            target = signed(self.read())
            self.stack.append((self.pc + target, self.pos, self.caps))
        elif bc == "L":
            target = signed(self.read())
            self.pc += target
            self.stack.append(self.pc)
        elif bc == "R":
            self.pc = self.stack.pop()
        elif bc == "M":
            target = signed(self.read())
            self.pc += target
            self.stack.pop()
        elif bc == "F":
            self.pc = fail
        else:
            raise Exception("Bad bytecode instruction %r" % bc)

    def run(self):
        try:
            while True:
                self.next()
        except Success:
            return True
        except Failure:
            return False

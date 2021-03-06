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
import os
import sys

from rpython.rlib.jit import JitDriver

from s.bytecode import (Instruction, Literal, Reference, Word, APPEND, DROP,
                        DUP, EJECT, ESCAPE, LIST, NEWE, OVER, SWAP,
                        MAKE_METHOD, OBJECT, CALL, SEND, IF, PRINT, ROT,
                        STACK)
from s.infer import infer_phrases
from s.lex import phrases_from_str
from s.objects import (E, Ejector, List, Promise, Str, UserObject,
                       unwrap_bool, unwrap_list, unwrap_str)


jitdriver = JitDriver(greens=["phrases", "token"], reds=["vm"])


def jitpolicy(driver):
    from rpython.jit.codewriter.policy import JitPolicy
    return JitPolicy()


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


class Machine(object):
    """
    A virtual machine which executes Eons.
    """

    def __init__(self, phrases):
        self.phrases = phrases

        self.stack = Stack()
        self.promises = []

    def pass_message(self, target, message, args):
        m = unwrap_str(message)
        a = unwrap_list(args)
        print "~ Passing to %s: %s, %s" % (target.repr(), message.repr(),
                args.repr())
        return target.call(m, a)

    def execute(self, token):
        jitdriver.jit_merge_point(phrases=self.phrases, token=token, vm=self)

        stack = self.stack

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
            elif i == NEWE:
                stack.push(E(self))
            elif i == DROP:
                stack.pop()
            elif i == DUP:
                stack.push(stack.peek())
            elif i == OVER:
                x = stack.pop()
                y = stack.peek()
                stack.push(x)
                stack.push(y)
            elif i == SWAP:
                x = stack.pop()
                y = stack.pop()
                stack.push(x)
                stack.push(y)
            elif i == ROT:
                z = stack.pop()
                y = stack.pop()
                x = stack.pop()
                stack.push(y)
                stack.push(z)
                stack.push(x)
            elif i == LIST:
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
            elif i == ESCAPE:
                target = stack.pop()
                ejector = Ejector(stack)
                stack.push(ejector)

                t = unwrap_str(target)
                with ejector:
                    self.run_phrase(t)
            elif i == EJECT:
                value = stack.pop()
                ejector = stack.pop()

                # Raise an exception to unwind the stack.
                ejector.eject(value)
            elif i == MAKE_METHOD:
                name = stack.pop()
                code = stack.pop()
                stack.push(List([name, code]))
            elif i == OBJECT:
                methods = stack.pop()

                obj = UserObject(self, methods)

                stack.push(obj)
            elif i == APPEND:
                obj = stack.pop()
                l = stack.peek()
                # Check the type.
                if isinstance(l, List):
                    l.push(obj)
                else:
                    raise TypeError("Couldn't push into non-List %s" % l)
            elif i == IF:
                otherwise = stack.pop()
                consequent = stack.pop()
                whether = unwrap_bool(stack.pop())

                if whether:
                    word = consequent
                else:
                    word = otherwise

                self.run_phrase(word._s)
            elif i == PRINT:
                print stack.pop().repr()
            elif i == STACK:
                print "Stack:", stack.repr()
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


def read_file(name):
    fd = os.open(name, os.O_RDONLY, 0777)
    l = []
    l.append(os.read(fd, 4096))
    while l[-1]:
        l.append(os.read(fd, 4096))
    return "".join(l)


def phrases_from_file(path):
    data = read_file(path)
    return phrases_from_str(data)


def entry_point(argv):
    print argv

    if len(argv) < 2:
        print "Not given any file to run..."
        return 1

    prelude = phrases_from_file("prelude.secret")
    phrases = phrases_from_file(argv[1])
    phrases.update(prelude)
    infer_phrases(phrases)

    if "main" in phrases:
        vm = Machine(phrases)
        vm.run("main")

    return 0


def target(*args):
    return entry_point, None


if __name__ == "__main__":
    entry_point(sys.argv)

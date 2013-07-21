import os
import sys

from s.bytecode import (builtins, Instruction, Literal, Reference, Word, DROP,
                        DUP, SWAP, ARGS, TO_ARG, MAKE_METHOD, OBJECT,
                        CALL, SEND, PRINT)
from s.lex import phrases_from_str
from s.objects import List, Promise, Str, UserObject


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


def read_file(name):
    fd = os.open(name, os.O_RDONLY, 0777)
    l = []
    l.append(os.read(fd, 4096))
    while l[-1]:
        l.append(os.read(fd, 4096))
    return "".join(l)


def entry_point(argv):
    if len(argv) < 2:
        print "Not given any file to run..."
        return 1

    data = read_file(argv[1])
    phrases = phrases_from_str(data)

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

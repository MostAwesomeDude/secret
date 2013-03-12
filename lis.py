from parsley import makeGrammar

g = """
ws = ' ' | '\\n'
digit = anything:x ?(x in "1234567890") -> x
number = digit+:ds -> int("".join(ds))
var = ws? (~ws anything)+:xs -> "".join(xs)
list = '(' thing?:car (ws thing)*:cdr ')' -> [car] + cdr if car else []
thing = list | number | var
program = ws? list:l ws? -> l
"""


class Env(dict):
    def __init__(self, parent):
        self.parent = parent

    def __missing__(self, k):
        return self.parent[k]

    def __setitem__(self, k, v):
        if k in self.parent:
            self.parent[k] = v
        else:
            dict.__setitem__(self, k, v)

    def define(self, k):
        dict.__setitem__(self, k, None)


i = """
quote :env = "quote" anything
ifthen :env = "if" anything:i anything:t anything:e
      -> interp(t if interp(i).eval(env) else e).eval(env)
set :env = "set!" anything:k anything:v
        -> env.__setitem__(k, interp(i).eval(env))
define :env = "define" anything:k anything:v
           -> env.define(k) or env.__setitem__(k, interp(i).eval(env))
procedure :env = "lambda" anything:vars anything:body
              -> (lambda *args:
    interp(body).eval(Env(env).update(dict(zip(vars, args)))))
begin :env = "begin" anything+:exprs
          -> [interp(expr).eval(env) for expr in exprs][-1]
eval :env = anything:proc anything*:args
         -> env[proc](*[interp(arg).eval(env) for arg in args])

main :env = quote(env) | ifthen(env) | set(env) | define(env)
          | procedure(env) | begin(env) | eval(env)
"""

parse = makeGrammar(g, {})
stuff = {"Env": Env}
interp = makeGrammar(i, stuff)
stuff["interp"] = interp

import sys

f = sys.stdin.read()
print repr(f)
p = parse(f).program()
print p
r = interp(p).main(Env({}))
print r

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

scheme = makeGrammar(g, {})

import sys

f = sys.stdin.read()
print repr(f)
print scheme(f).program()

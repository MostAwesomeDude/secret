from terml.nodes import termMaker as t
from ometa.grammar import OMeta

g = open("python.parsley").read()

class Parser(OMeta.makeGrammar(g, globals())):
    pass

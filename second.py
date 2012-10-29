from ometa.boot import BootOMetaGrammar
from terml.nodes import termMaker as t

from first import PythonParser, join

g = open("second.parsley").read()

class SecondParser(BootOMetaGrammar.makeGrammar(g, globals()), PythonParser):
    pass

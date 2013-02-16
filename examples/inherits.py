o = object

def f():
    return object

class HasDepth(object, o):
    depth = 0

class InheritsFromExpression(f()):
    depth = 0

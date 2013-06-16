builtins = {
    "()":      (0, 1),
    ".":       (3, 1),
    "<-":      (3, 1),
    "<method": (2, 1),
    "drop":    (1, 0),
    "dup":     (1, 2),
    "object":  (1, 1),
    "print":   (1, 0),
    "push":    (2, 1),
    "swap":    (2, 2),
}


def combine_stack_effects(fi, fo, si, so):
    if fo > si:
        so += fo - si
    elif si > fo:
        fi += si - fo
    return fi, so


def infer_stack_effect(tokens):
    i = o = 0

    for token in tokens:
        if token in builtins:
            ni, no = builtins[token]
            i, o = combine_stack_effects(i, o, ni, no)
        else:
            o += 1

    return i, o


def execute(token, stack, context):
    if False:
        pass
    elif token == "()":
        stack.push([])
    elif token == ".":
        args = stack.pop()
        name = stack.pop()
        target = stack.pop()
        result = pass_message(target, name, args)
        stack.push(result)
    elif token == "<-":
        args = stack.pop()
        name = stack.pop()
        target = stack.pop()
        # XXX wrong
        stack.push((target, name, args))
    elif token == "<method":
        name = stack.pop()
        code = stack.pop()
        stack.push((name, code))
    elif token == "drop":
        stack.pop()
    elif token == "dup":
        stack.push(stack.peek())
    elif token == "object":
        methods = stack.pop()
        d = dict(methods)
        stack.push(d)
    elif token == "push":
        obj = stack.pop()
        l = stack.peek()
        l.append(obj)
    elif token == "print":
        print stack.pop()
    elif token == "swap":
        x = stack.pop()
        y = stack.pop()
        stack.push(x)
        stack.push(y)


def parse_pieces(data):
    pieces = data.split()
    return filter(bool, pieces)


def parse_phrases(pieces):
    phrases = {}

    while pieces:
        try:
            start = pieces.index(":")
            end = pieces.index(";")
        except ValueError:
            break

        name = pieces[start + 1]
        phrase = pieces[start + 2:end]
        phrases[name] = phrase

        pieces = pieces[end + 1:]

    return phrases


def main(name):
    data = open(name, "rb").read()
    pieces = parse_pieces(data)
    phrases = parse_phrases(pieces)

    for word, phrase in phrases.items():
        print "Word:", word
        print "Tokens:", " ".join(phrase)
        print "Stack effect:", infer_stack_effect(phrase)


if __name__ == "__main__":
    import sys
    main(sys.argv[1])

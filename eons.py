def split_tokens(s):
    return s.split(" ")

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

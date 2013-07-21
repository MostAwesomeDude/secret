from s.bytecode import bytecodes, Instruction, Literal, Reference, Word
from s.objects import Bool, Int, Str


def classify(x):
    if x in bytecodes:
        return Instruction(bytecodes[x])
    else:
        try:
            return Literal(Int(int(x)))
        except ValueError:
            pass

        if x.startswith("\"") and x.endswith("\""):
            if len(x) > 2:
                # RPython has trouble with this bit of math.
                end = len(x) - 1
                assert end > 1
                return Literal(Str(x[1:end]))
            else:
                # Empty string: "" or "
                return Literal(Str(""))

        if x == "true":
            return Literal(Bool(True))
        if x == "false":
            return Literal(Bool(False))

        if x.startswith("*") and len(x) > 1:
            return Reference(x[1:])

    return Word(x)


def parse_pieces(data):
    lines = data.split("\n")
    pieces = []
    for line in lines:
        pieces.extend(line.split(" "))
    filtered = [piece for piece in pieces if piece]

    return filtered


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


def classify_phrases(phrases):
    d = {}
    for word in phrases.keys():
        d[word] = [classify(w) for w in phrases[word]]
    return d


def phrases_from_str(s):
    pieces = parse_pieces(s)
    phrases = classify_phrases(parse_phrases(pieces))
    return phrases

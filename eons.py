def split_tokens(s):
    return s.split(" ")

builtins = {
    "()":    (0, 1),
    ".":     (3, 1),
    "<-":    (3, 1),
    "drop":  (1, 0),
    "dup":   (1, 2),
    "print": (1, 0),
    "push":  (2, 1),
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

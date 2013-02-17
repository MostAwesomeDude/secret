def join(separator, seq):
    "A Twine-friendly string join."
    if not seq:
        return ''
    return seq[0].__class__(separator).join(seq)

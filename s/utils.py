def all(iterable):
    """
    Determine whether everything in an iterable is truthy.
    """

    rv = True
    for item in iterable:
        if not item:
            rv = False
    return rv

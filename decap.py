#!/usr/bin/env python

import struct
import sys

word = struct.Struct("<Q")
long = struct.Struct("<I")


def find_segments(buf):
    count, = long.unpack_from(buf)
    count += 1

    offset = count * 4
    # Round up to the next word boundary.
    if count % 2:
        offset += 4

    l = []
    for i in range(count):
        length, = long.unpack_from(buf, offset=(i + 1) * 4)
        l.append(offset)
        offset += length * 8

    return l


def resign(i):
    if i & 0x20000000:
        i -= 0x40000000
    return i


def pointer_type(w):
    return w >> 62


def struct_pointer(w):
    return resign(w >> 32 & 0x3fffffff), w >> 16 & 0xffff, w & 0xffff


def list_pointer(w):
    return resign(w >> 32 & 0x3fffffff), w >> 29 & 0x7, w & 0x1fffffff


buf = sys.stdin.read()

segments = find_segments(buf)
print "Segments:", segments

root, = word.unpack_from(buf, offset=segments[0])

print "Root:", hex(root)
print "Type:", pointer_type(root)
print "Struct pointer:", struct_pointer(root)

offset, data, ptrs = struct_pointer(root)

offset += 1

ptrs, = word.unpack_from(buf, offset=offset * 8)

print "Type:", pointer_type(ptrs)
print "Struct pointer:", struct_pointer(ptrs)

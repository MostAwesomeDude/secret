#!/usr/bin/env python
# Copyright (C) 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.

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

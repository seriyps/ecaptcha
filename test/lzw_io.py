#!/usr/bin/env python3

from gif import lzw_compress
import sys

if __name__ == "__main__":
    # f = open(sys.argv[1], "rb")
    lzw_min = sys.stdin.buffer.read(1)[0]
    raw_bytes = sys.stdin.buffer.read()
    sys.stdout.buffer.write(lzw_compress(bytes(raw_bytes), lzw_min))

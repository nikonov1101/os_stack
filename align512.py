block_size = 512

import sys
k = "early.bin"
if len(sys.argv) > 1:
    k = sys.argv[1]

print(f"aligning '{k}' to 512 bytes")

with open(k, 'rb') as f:
   bs = f.read()

sz = len(bs)
if sz == 0:
    print(f"{k} is empty, pleaes rebuild.")
    sys.exit(1)

if sz % block_size == 0:
    print(f"size is {sz}, no padding needed.")
    sys.exit(0)

blocks = 1 + (sz // block_size)
pad_to = block_size * (blocks)
more = pad_to - sz
print(f"{blocks} blocks, pad to {pad_to}, padding with {more} more bytes")

pad = bytearray(more)
with open(k, 'ab') as f:
    f.write(pad)



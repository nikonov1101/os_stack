import struct

# in gdb:
# 1. set breakpoint at forever
# 2. dump memory e820.bin 0xc00 0xc94
# where 0xc00 is a start of e820 section (see memmap in readme)
# and 0xc94 is a base address
#     plus 6 times 24 for table records
#     plus 4 for the record count itself
filename = './e820.bin'

with open(filename, 'rb') as f:
    raw_bytes = f.read(4)
    (n_rec,) = struct.unpack('<I', raw_bytes)
    print(f"table size: {n_rec}")

    for i in range(n_rec):
        # start64
        raw_bytes = f.read(8)
        (start64,) = struct.unpack('<Q', raw_bytes)

        # offset64
        raw_bytes = f.read(8)
        (offset64,) = struct.unpack('<Q', raw_bytes)

        # type32
        raw_bytes = f.read(4)
        (addrType,) = struct.unpack('<I', raw_bytes)
        typ = "n/a"
        if addrType == 1:
            typ = "RAM"
        sk = offset64 // 1024

        print(f"0x{start64:08x} - 0x{offset64:08x} : {typ} : {sk}K ")

        _ = f.read(4) # padding

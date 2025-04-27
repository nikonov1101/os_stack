MBR_END = 509 # whole mbr without magic bytes

with open("bootloader.bin", "rb") as f:
    bs = f.read()
    i = MBR_END
    while i> 0:
        if bs[i] != 0:
            break
        i -= 1
    print(f"{MBR_END - i} free bytes in MBR sector" )


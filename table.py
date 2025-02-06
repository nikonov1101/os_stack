base = 0x7c00 # same as [ORG addr] in the bootloader' code

with open("./symbols.table", "r") as f:
    flag = False
    for line in f.readlines():
        if not flag and line == 'SYMBOL TABLE:\n':
            flag = True
            continue

        if flag:
            line = line[:len(line)-1] # remove \n
            parts = line.split()
            if len(parts) > 0:
                addri = 0
                symi = 4
                if len(parts) == 6:
                    symi = 5
                offset = parts[addri]
                sym = parts[symi]

                la = int(offset, 16) + base
                gdb_cmd = f"break {hex(la)}"

                print(f"{offset} :: {sym}\t\t{gdb_cmd}")

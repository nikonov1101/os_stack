set arch i8086 
set disassembly-flavor intel
target remote :1234
break *0x7c00

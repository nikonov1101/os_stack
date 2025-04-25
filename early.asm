[bits 32]
[extern k_early] ; Define calling point. Must have same name as kernel.c 'main' function
call k_early ; Calls the C function. The linker will know where it is placed in memory
jmp $


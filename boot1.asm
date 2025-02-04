;tell the assembler that its a 16 bit code
[BITS 16]

;Origin, tell the assembler that where the code will
;be in memory after it is been loaded
[ORG 0x7C00]

start:
    cli ; clear interrupts
	xor	AX, AX			;Set segment registers to zero
	mov	ES, AX
	mov	DS, AX
	mov	SS, AX
	mov	SP, start		;Top of stack
	mov	DI, SP			;  is bottom of relocation point

    ; set video mode
    ; assuming AH = 0
    mov AL, 0x12 ;  (16-color 640x480)
    int 0x10
    ; set cursor position
    mov AH, 0x02
    mov BH , 0x00
    mov DH, 0
    mov DL, 0
    int 0x10

main:
    mov SI, str_hello ; SI points to the beginning of the string
    call    print_string

exit:
    JMP $ 		;infinite loop

;Assume that ASCII value is in register AL
print_chr:
    mov AH, 0x0E ; bios procedure number
    mov BH, 0x00 ; page number
    ; bit  7 - blink
    ;Bits 6–4 = Background color in RGB order (3 bits).
        ; Bit 6 = Red (background)
        ; Bit 5 = Green (background)
        ; Bit 4 = Blue (background)
    ; Bits 3–0 = Foreground color in RGB + “Intensity” order (4 bits).
        ; Bit 3 = Intensity (sometimes called “bright” bit)
        ; Bit 2 = Red (foreground)
        ; Bit 1 = Green (foreground)
        ; Bit 0 = Blue (foreground)
    mov BL, 0x4C; text attributes
    int 0x10  ; call bios procedure
    ret

; assume string pointer in SI
print_string:
    mov AL, [SI] ; chr = str[si]
    or AL, al ; null-termintor? chr == 0 ?
    jz return

    call print_chr

    inc SI  ; i++
    jmp print_string


print_ascii:
    ; A, start of the ascii table
    mov AL, 33
print_ascii_loop:
    call print_chr

    inc AL  ; next char
    cmp AL, 127 ; end of ascii table
    je return

    jmp print_ascii_loop

return:
    ret



; data
str_hello db 'Hello, bootloader!', 0

TIMES 510 - ($ - $$) db 0	;fill the rest of sector with 0
DW 0xAA55			; add boot signature at the end of bootloader

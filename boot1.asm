;tell the assembler that its a 16 bit code
[BITS 16]

;Origin, tell the assembler that where the code will
;be in memory after it is been loaded
[ORG 0x7C00]

stage1:
    cli ; clear interrupts
	xor	AX, AX			;Set segment registers to zero
	mov	ES, AX
	mov	DS, AX
	mov	SS, AX
	mov	SP, stage1		;Top of stack
	mov	DI, SP			;  is bottom of relocation point

    ; set video mode, assuming AH = 0
    mov AL, 0x12 ;  (16-color 640x480)
    int 0x10

    ; set the cursor position
    mov AH, 0x02
    mov BH, 0x00
    mov DX, 0x00
    int 0x10

main:
    mov SI, str_hello ; SI points to the beginning of the string
    mov BL, 0x03      ; text color
    call    print_string

    mov	si, 3			; for i < 3, do:
load_stage2:
    ; read next block from a boot disk into the memory
    mov ax, 0x0201; read one sector command
    mov cx, 0x02 ; read SECOND block from disk, the first one is THIS one
    mov dx, 0x00 ; DL points to a floppy0 aka disk A:

    mov bx, stage2 ; stage1+512 must be equal to stage2 addr
    int 0x13;

	jnc	read_ok			;Success
	dec	si			;retry count
	jnz	load_stage2

load_err:
    ; handle error and hang forever
    mov si, str_err
    mov BL, 0x04 ; error color
    call print_string
    jmp $;

read_ok:
    ; check the stage2 signature
    cmp word[stage2+510], 0xDEAD
    jne load_err

verify_ok:
    mov si, str_stage2_found
    mov BL, 0x07
    call print_string
    jmp stage2


; Assume that ASCII value is in register AL
; assume text color options is register BL
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
;    mov BL, 0x4C; text attributes
    int 0x10  ; call bios procedure
    ret

; assume string pointer in SI
print_string:
    mov AL, [SI] ; chr = str[si]
    or AL, AL ; null-termintor? chr == 0 ?
    jz return

    call print_chr

    inc SI  ; i++
    jmp print_string

return:
    ret

; assume input byte in AL
; MOD AX, BX
byte_to_char:
    push ax
    mov bx, ax

    ; deal with a first character
    and bx, 0x00F0 ; leave only hi bytes
    shr bx, 4       ; move significant bits to the right
    add bx, tab_hextoc ; now offset in BL, add the base address of the table
    mov al, byte[bx]   ; reference a byte pointed by BX, load into AL
    mov bl, 0x07
    call print_chr ; print a byte from AL


    ; deal with the second char
    pop ax
    mov bx, ax  ; restore AX, copy into BX for modifications
    and bx, 0x000f ; remove all but LSB, got offset of the LSB sigit
    ; the rest is the same as above
    add bx, tab_hextoc
    mov al, byte[bx]
    mov bl, 0x07
    call print_chr

    ret

; assume word is in AX
print_word:
   push ax
   shr ax, 8 ;; shift AH into AL
   call byte_to_char

   pop ax
   and ax, 0x00ff
   call byte_to_char

   ret


; stage1 data,
; note that printing \r \n (10, 13) via bios procedures
; corretly move the cursor on a next line, so you don;t have to advance
; the cursor position by hand, which is nice.
str_hello db 'stage1 bootloader started', 10, 13 , 0
str_err db 'Error loading stage2', 10, 13, 0
str_stage2_found db 'stage2 bootloader found, passing control', 10, 13, 0
tab_hextoc db '0123456789ABCDEF'

; TODO: where should we place the partition table?

times 510 - ($ - $$) db 0	;fill the rest of sector with 0
dw 0xAA55			; add boot signature at the end of bootloader
; --- MBR end ---

; stage2 starts here: the idea is to put stage2 bootloader right after the MBR,
; load it via bios procedure, and pass control to a newly loaded block.
stage2:
    mov SI, str_stage2
    mov BL, 0x02
    call print_string

    ; print in advance, src=AX  will be trashed
    mov SI, str_lowmem
    call print_string

lowmem_detect:
    ; detect low memory
    clc
    int 0x12 ; request low memory size
    ; AX = amount of continuous memory in KB starting from 0.
    ; The carry flag is set if it failed
    jc lowmem_err

t_lowmem_handle:  ; just for offset calculation
    call print_word
    mov al, 'k'
    call print_chr
    jmp forever


lowmem_err:
    mov SI, str_errcode
    call print_string
    mov al, '1' ; how to define all codes in one place?
    call print_chr
    jmp forever

forever:
    jmp $


; stage2 data
str_stage2 db 'stage2 bootloader started', 10, 13, 0
str_errcode db 'error code: ', 0
str_lowmem db 'low mem: ', 0

times 510 - ($ - stage2) db 0 ;; padding
dw 0xDEAD

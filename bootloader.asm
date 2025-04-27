%define BIOS_VIDEO_BASE 0xB8000
%define BIOS_SCREEN_WIDTH 0x44A
%define BIOS_SCREEN_ROWS 0x451
%define BIOS_SCREEN_COLS 0x450

%define KERNEL_EARLY_AT      0x8000 ; where to load early32 kernel
%define KERNEL_BLOCKS        %!KERNEL_BLOCKS

;;; error codes
%define ERR_LOAD_FAILED 0x0101
%define ERR_BAD_CHECKSUM 0x0102
%define ERR_LOWMEM_ERR 0x0201

%define HANDOVER_BOOT_DEVICE 0x600 ; WORD
%define HANDOVER_LOWMEM_SZ_K 0x602 ; WORD
%define HANDOVER_VIDEO_MEM_OFFSET 0x604 ; DOUBLE WORD,cursor offset in a video mem


[BITS 16]
[ORG 0x7C00]
_start_boot0:
    cli ; clear interrupts
    ; canonicalizing CS:IP
    ; as of https://www.cs.cmu.edu/~410-s07/p4/p4-boot.pdf
    ; section 6.1
    jmp 0x00:boot0
boot0:
	xor	AX, AX			;Set segment registers to zero
	mov	ES, AX
	mov	DS, AX
	mov	SS, AX
	mov	SP, _start_boot0  ;Top of stack

    ; save the boot disk pointer in hand-over table
    mov word [HANDOVER_BOOT_DEVICE], dx
    push dx ; will be trashed by int 10h right below.

    ; set video mode, assuming AH = 0
    mov AL, 0x03 ;  (80x25, color)
    int 0x10

    ; set the cursor position
    mov AH, 0x02
    mov BH, 0x00
    mov DX, 0x00
    int 0x10

.main:
    mov si, boot0data.str_hello ; SI points to the beginning of the string
    call print_string

detect_lowmem:
    clc
    int 0x12 ; request low memory size
    ; AX = amount of continuous memory in KB starting from 0.
    ; The carry flag is set if it failed
    jnc .lowmem_done

.lowmem_err:
    mov ax, ERR_LOWMEM_ERR
    jmp print_err

.lowmem_done:
    ; save AL in the handover area
    mov [HANDOVER_LOWMEM_SZ_K], ax

set_a20:
    ; enable A20 line
    in al, 0x92
    or al, 2
    out 0x92, al

load_early32:
.reset_drive:
    pop dx
    ; TODO: should I set ah=0x0d if we boot from a hard drive?
    mov ah, 0x00
    int 0x13

    mov	si, 3			; for i < 3, do:
.read_loop:
    ; read next blocks from a boot disk into the memory
    mov ah, 0x02              ; read command
    mov al, KERNEL_BLOCKS     ; number of sectors to read
    mov cx, 0x02              ; read SECOND block from disk, the first one is THIS one (with mbr).
    ; dx already contains the drive number we've boot from

    ; ES:BX = pointer to buffer
    mov bx, KERNEL_EARLY_AT
    int 0x13;
	jnc	setup_gdt ; no error, we're done

    dec si
	jnz	.read_loop

.load_err:
    mov ax, ERR_LOAD_FAILED
    jmp print_err

setup_gdt:
    xor ax, ax
    mov ds, ax
    lgdt [gdt.desc]

setup_pmode:
    mov eax, cr0
    or eax, 1
    mov cr0, eax

    ; preserve the offset before protected mode
    call detect_cursor_pos
    .dig_here:
    jmp code32:reload_CS

;;; utils
; Assume that ASCII value is in register AL
print_chr:
    mov ah, 0x0E ; bios procedure number
    mov bh, 0x00 ; page number
    mov bl, 0x07 ; no fancy colors yet, ignore caller colors as well
    int 0x10  ; call bios procedure
    ret

; assume string pointer in SI
print_string:
    mov al, [si] ; chr = str[si]
    or al, al ; is null-termintor?
    jz .return

    call print_chr

    inc si  ; i++
    jmp print_string
.return:
    ret

; assume input byte in AL
; MOD AX, BX
byte_to_char:
    push ax
    mov bx, ax

    ; deal with a first character
    and bx, 0x00F0 ; leave only hi bytes
    shr bx, 4       ; move significant bits to the right
    add bx, boot0data.tab_hextoc ; now offset in BL, add the base address of the table
    mov al, byte[bx]   ; reference a byte pointed by BX, load into AL
    call print_chr ; print a byte from AL

    ; deal with the second char
    pop ax
    mov bx, ax  ; restore AX, copy into BX for modifications
    and bx, 0x000f ; remove all but LSB, got offset of the LSB sigit
    ; the rest is the same as above
    add bx, boot0data.tab_hextoc
    mov al, byte[bx]
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

; assume error code in AX:
;    AH - sub-system ID, references a part of a boot process
;    AL - the error code itself
print_err:
    push ax

    mov si, boot0data.str_err
    call print_string

    pop ax
    call print_word
    jmp $ ; hang forever


;; video memory offset
;; return (width * 2) * rows + rows in CX
; trashes AX, CX
detect_cursor_pos:
    pusha

    xor ecx, ecx
    xor ax, ax

    mov cx, word[BIOS_SCREEN_WIDTH]
    ;;; TODO: does Left Shift by 1 is cheaper?
    imul cx, 2          ; chars are 2-byte wide: ascii, color

    mov al, byte[BIOS_SCREEN_ROWS]
    imul cx, ax

    mov al, byte[BIOS_SCREEN_COLS]
    add cx, ax

    add ecx, BIOS_VIDEO_BASE
    mov dword[HANDOVER_VIDEO_MEM_OFFSET], ecx

    popa
    ret

; boot0 data,
; note that printing \r \n (10, 13) via bios procedures
; corretly move the cursor on a next line, so you don;t have to advance
; the cursor position by hand, which is nice.
boot0data:
    .str_hello  db 'boot0 started', 10, 13 , 0
    .str_err    db 'ERR: ', 0
    .tab_hextoc db '0123456789ABCDEF'

; early mode GDT lives here
; http://web.archive.org/web/20190424213806/http://www.osdever.net/tutorials/view/the-world-of-protected-mode
gdt:
.null:
    dq  0
.code:
    dw 0x0FFFF
    dw 0
    db 0; continue of the base address
    db 10011010b ; ring0-only readable code segment, nonconforming
    db 11001111b ; 32-bit code, 4kb segment
    db 0
.data:
    dw 0x0FFFF
    dw 0
    db 0; continue of the base address
    db 10010010b ; ring0-only writeable data segment, expand down
    db 11001111b ; same as for code segment
    db 0
.end:
.desc:
   dw .end - gdt -1 ; number of records
   dd gdt          ;  start of the table

code32 equ gdt.code - gdt
data32 equ gdt.data - gdt


[bits 32]
reload_CS:
    mov ax, data32
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    ; stack early
    mov ss, ax
    mov ebp, 0x9c000
    mov esp, ebp

    ;;; 32-bit early mode starts here,
    call code32:KERNEL_EARLY_AT
    hlt

times 510 - ($ - $$) db 0	;fill the rest of sector with 0
boot0_signature dw 0xAA55			; add boot signature at the end of bootloader

; vim: filetype=nasm

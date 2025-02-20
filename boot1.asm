;tell the assembler that its a 16 bit code
[BITS 16]

;Origin, tell the assembler that where the code will
;be in memory after it is been loaded
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
	mov	SP, _start_boot0 ;Top of stack
	mov	DI, SP			  ;is bottom of relocation point

    ; save the boot disk pointer in DL
    push dx

    ; set video mode, assuming AH = 0
    mov AL, 0x12 ;  (16-color 640x480)
    int 0x10

    ; set the cursor position
    mov AH, 0x02
    mov BH, 0x00
    mov DX, 0x00
    int 0x10

.main:
    mov SI, boot0data.str_hello ; SI points to the beginning of the string
    mov BL, 0x03      ; text color
    call    print_string

    ;
    ; print boot device number
    ;
    mov si, boot0data.str_bootdrive
    call print_string

    pop dx ; DO NOT MODIFY DX UNTIL BOOT1
    mov al, dl
    call byte_to_char

    ; \r \n
    mov al, 10
    call print_chr
    mov al, 13
    call print_chr

.load_boot1:
.reset_drive:
    ; TODO: should I set ah=0x0d if we boot from a hard drive?
    mov ah, 0x00
    int 0x13

    mov	si, 3			; for i < 3, do:
.read_boot1:
    ; read next block from a boot disk into the memory
    mov ah, 0x02    ; read command
    mov al, 0x01    ; sector count
    mov cx, 0x02 ; read SECOND block from disk, the first one is THIS one
    ; dx already contains the drive number we've boot from

    ; ES:BX = pointer to buffer
    mov bx, boot1 ; boot0+512 must be equal to boot1 addr
    int 0x13;
	jnc	.verify_boot1			;Success

    dec si
	jnz	.read_boot1

.load_err:
    ; handle error and hang forever
    mov si, boot0data.str_err
    mov bl, 0x04 ; error color
    call print_string
    jmp $;

.verify_boot1:
    cmp word[boot1data.signature], 0xDEAD
    jne .load_err

    ; check boot1 size, if 1 sector
    ; then we already have it loaded, jump to boot1
    cmp byte[boot1data.size_sectors], 1
    je .boot1_fully_loaded

    ;
    ; size-1 = number of sectors to load

    dec byte[boot1data.size_sectors]
    mov al, byte[boot1data.size_sectors]

    mov si, 3 ; i = 3
.load_boot1_more:
    ; bootloader is larger than 1 sector, go load the rest

    ; read next block from a boot disk into the memory
    mov cx, 0x03  ; s1 = mbr, s2=boot1_start, read the rest starting from s3
    ; 	ES:BX = pointer to buffer
    mov bx, boot1+512 ; offset for next sectors
    mov ah, 0x02    ; read command
    ; dx already contains the drive number we've boot from

    int 0x13;
	jnc	.boot1_fully_loaded			;Success

    ; reload al, try again
    dec si
    jz .load_err

    mov al, byte[boot1data.size_sectors]
	jmp	.load_boot1_more

.boot1_fully_loaded:
    mov si, boot0data.str_found
    mov BL, 0x07
    call print_string
    jmp boot1


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
    mov BL, 0x4C; text attributes
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
    add bx, boot0data.tab_hextoc ; now offset in BL, add the base address of the table
    mov al, byte[bx]   ; reference a byte pointed by BX, load into AL
    mov bl, 0x07
    call print_chr ; print a byte from AL


    ; deal with the second char
    pop ax
    mov bx, ax  ; restore AX, copy into BX for modifications
    and bx, 0x000f ; remove all but LSB, got offset of the LSB sigit
    ; the rest is the same as above
    add bx, boot0data.tab_hextoc
    mov al, byte[bx]
    mov bl, 0x07
    call print_chr

    ret

debug_ax:
    push ax
    call print_chr
    mov ax, 0x0a
    call print_chr
    mov ax, 0x0d
    call print_chr
    pop ax
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


; boot0 data,
; note that printing \r \n (10, 13) via bios procedures
; corretly move the cursor on a next line, so you don;t have to advance
; the cursor position by hand, which is nice.
boot0data:
.str_hello db 'boot0 bootloader started', 10, 13 , 0
.str_bootdrive db 'boot drive: ', 0
.str_err db 'Error loading boot1', 10, 13, 0
.str_found db 'boot1 found, passing control', 10, 13, 0
.tab_hextoc db '0123456789ABCDEF'

times 510 - ($ - $$) db 0	;fill the rest of sector with 0
.mbr_signature dw 0xAA55			; add boot signature at the end of bootloader

; --- MBR end ---

; boot1 starts here: the idea is to put boot1 code right after the MBR,
; load it via bios procedure, and pass control to a newly loaded block.
; offset from .ORG is 512 byte to match the size of a disk sector
boot1:
    mov SI, boot1data.str_hello
    mov BL, 0x02
    call print_string

    ; print in advance, src=AX  will be trashed
    mov SI, boot1data.str_lowmem
    call print_string

.lowmem_detect:
    ; detect low memory
    clc
    int 0x12 ; request low memory size
    ; AX = amount of continuous memory in KB starting from 0.
    ; The carry flag is set if it failed
    jc .lowmem_err

.lowmem_print:
    call print_word
    mov al, 'k'
    call print_chr

.set_a20:
    mov si, boot1data.str_a20_state
    call print_string
    call get_a20_state
    push ax
    add ax, '0'
    call print_chr
    pop ax
    jmp forever

.lowmem_err:
    mov SI, boot1data.str_errcode
    call print_string
    mov al, '1' ; how to define all codes in one place?
    call print_chr
    jmp forever

forever:
    jmp $

;	out:
;		ax - state (0 - disabled, 1 - enabled)
get_a20_state:
	pushf
	push si
	push di
	push ds
	push es

	mov ax, 0x0000					;	0x0000:0x0500(0x00000500) -> ds:si
	mov ds, ax
	mov si, 0x0500

	not ax							;	0xffff:0x0510(0x00100500) -> es:di
	mov es, ax
	mov di, 0x0510

	mov al, [ds:si]					;	save old values
	mov byte [.BufferBelowMB], al
	mov al, [es:di]
	mov byte [.BufferOverMB], al

	mov ah, 1						;	check byte [0x00100500] == byte [0x0500]
	mov byte [ds:si], 0
	mov byte [es:di], 1
	mov al, [ds:si]
	cmp al, [es:di]
	jne .exit
	dec ah
.exit:
	mov al, [.BufferBelowMB]
	mov [ds:si], al
	mov al, [.BufferOverMB]
	mov [es:di], al
	shr ax, 8
	pop es
	pop ds
	pop di
	pop si
	popf
	ret

.BufferBelowMB:	db 0
.BufferOverMB	db 0

; boot1 data
boot1data:
.str_hello db 'boot1 started', 10, 13, 0
.str_errcode db 'error code: ', 0
.str_lowmem db 'low mem: ', 0
.str_a20_state db 10,13,'a20: ', 0
.size_sectors db 1

times 510 - ($ - boot1) db 0 ;; padding
.signature dw 0xDEAD


; stage 3.5 test code starts below
stage3:
    ;;; try to load this into hi mem
    mov si, .hello
    call print_string
    jmp $
; Assume that ASCII value is in register AL
; assume text color options is register BL
.print_chr:
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
.print_string:
    mov AL, [SI] ; chr = str[si]
    or AL, AL ; null-termintor? chr == 0 ?
    jz .return

    call .print_chr

    inc SI  ; i++
    jmp .print_string

.return:
    ret

.hello db 'boot1s2 loaded', 10, 13, 0
times 510 - ($ - stage3) db 0 ;; padding
dw 0xFACE

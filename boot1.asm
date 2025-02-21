;tell the assembler that its a 16 bit code
[BITS 16]

;Origin, tell the assembler that where the code will
;be in memory after it is been loaded
[ORG 0x7C00]

; handover table starts at 0x600
boot_device_p equ 0x600 ; word, content of dx right after boot: 0 for fda, 0x80 for hda
lowmem_p equ 0x602      ; word, result of int 0x12: amount of continuous memory in KB starting from 0.
;;; TODO: place tmpGDT BEFORE e820 maps, because GDT is of known and fixed size
;;; TODO: is there a limit of entities in e820 table?
e820_table_p equ 0xC00 ; himem maps

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
    mov word [boot_device_p], dx
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
    mov bl, 0x07 ; no fancy colors yet, ignore caller colors as well
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

debug_ax:
    push ax
    call print_chr
    mov ax, 10 ; \r
    call print_chr
    mov ax, 13 ; \n
    call print_chr
    pop ax
    ret
crlf:
    mov al, 10
    call print_chr
    mov al, 13
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

    ;mov ax, 10
    ;call print_chr
    ;mov ax, 13
    ;call print_chr

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
    jnc .lowmem_print

.lowmem_err:
    mov SI, boot1data.str_errcode
    call print_string
    mov al, '1' ; how to define all codes in one place?
    call print_chr
    jmp forever

.lowmem_print:
    ; save AL in the handover area
    mov [lowmem_p], ax
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

detech_himem:
; referencing http://www.uruk.org/orig-grub/mem64mb.html
; referencing https://wiki.osdev.org/Detecting_Memory_(x86)
; [es:si] points to a buffer
; move 4 bytes further to keep a space for a table size
    mov di, e820_table_p+4
	xor ebx, ebx		; ebx must be 0 to start
	xor bp, bp		; keep an entry count in bp
	mov edx, 0x0534D4150	; Place "SMAP" into edx
	mov eax, 0xe820
	mov [es:di + 20], dword 1	; force a valid ACPI 3.X entry
	mov ecx, 24		; ask for 24 bytes
	int 0x15

	jc .failed	; carry set on first call means "unsupported function"
	mov edx, 0x0534D4150	; Some BIOSes apparently trash this register?
	cmp eax, edx		; on success, eax must have been reset to "SMAP"
	jne .failed
	test ebx, ebx		; ebx = 0 implies list is only 1 entry long (worthless)
	je .failed
	jmp .check_20b_entry

.read_next:
	mov eax, 0xe820		; eax, ecx get trashed on every int 0x15 call
	mov [es:di + 20], dword 1	; force a valid ACPI 3.X entry
	mov ecx, 24		; ask for 24 bytes again
	int 0x15
	jc short .write_rec_count		; carry set means "end of list already reached"
	mov edx, 0x0534D4150	; repair potentially trashed register
.check_20b_entry:
	jcxz .check_table_end		; skip any 0 length entries
	cmp cl, 20		; got a 24 byte ACPI 3.X response?
	jbe short .got_20b_entry
	test byte [es:di + 20], 1	; if so: is the "ignore this data" bit clear?
	je short .check_table_end
.got_20b_entry:
	mov ecx, [es:di + 8]	; get lower uint32_t of memory region length
	or ecx, [es:di + 12]	; "or" it with upper uint32_t to test for zero
	jz .check_table_end		; if length uint64_t is 0, skip entry
	inc bp			; got a good entry: ++count, move to next storage spot
	add di, 24
.check_table_end:
	test ebx, ebx		; if ebx resets to 0, list is complete
	jne .read_next

.write_rec_count:
	mov [es:e820_table_p], bp	; store the entry count
	clc			; there is "jc" on end of list to this point, so the carry must be cleared
	jmp .print_himem2
.failed:
    mov bp, 0
	stc			; "function unsupported" error exit

.print_himem2:
    clc ; just in case
    mov si, boot1data.str_e820_regions
    call print_string

    mov ax, bp
    call print_word
    call crlf

; assume BX points to the start of the e820 entry
    mov cx, e820_table_p+4
    mov si, 0
.print_segment:
    mov bx, cx
    mov ax, word[bx]
    call print_word
    call crlf

    add si, 2
    cmp si, 24; end of entry?
    je .next_segment; jump if negative
    add cx, 2
    jmp .print_segment

.next_segment:
    mov al, '@'
    call print_chr
    call crlf
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
	mov byte [boot1data.BufferBelowMB], al
	mov al, [es:di]
	mov byte [boot1data.BufferOverMB], al

	mov ah, 1						;	check byte [0x00100500] == byte [0x0500]
	mov byte [ds:si], 0
	mov byte [es:di], 1
	mov al, [ds:si]
	cmp al, [es:di]
	jne .exit
	dec ah
.exit:
	mov al, [boot1data.BufferBelowMB]
	mov [ds:si], al
	mov al, [boot1data.BufferOverMB]
	mov [es:di], al
	shr ax, 8
	pop es
	pop ds
	pop di
	pop si
	popf
	ret

; boot1 data
boot1data:
.BufferBelowMB:	db 0
.BufferOverMB	db 0
.str_hello db 'boot1 started', 10, 13, 0
.str_errcode db 'error code: ', 0
.str_lowmem db 'low mem: ', 0
.str_a20_state db 10,13,'a20: ', 0
.str_e820_regions db 10,13,'e820 reg: ',0
.size_sectors db 1 ; TODO: relocate data section on expantion?

times 510 - ($ - boot1) db 0 ;; padding
.signature dw 0xDEAD


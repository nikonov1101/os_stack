%define KERNEL_BLOCK_SZ 1

;tell the assembler that its a 16 bit code
[BITS 16]

;Origin, tell the assembler that where the code will
;be in memory after it is been loaded
[ORG 0x7C00]

; handover table starts at 0x600
boot_device_p equ 0x600 ; word, content of dx right after boot: 0 for fda, 0x80 for hda
lowmem_p equ 0x602      ; word, result of int 0x12: amount of continuous memory in KB starting from 0.
int15_c0_table_p equ 0x604; dword, points to https://stanislavs.org/helppc/int_15-c0.html results

; TODO make it continous area?
cpuid1_p equ 0x608 ; word, EBX after cpuid call
cpuid2_p equ 0x60a ; word, EDX after cpuid call
cpuid3_p equ 0x60c ; word, ECX after cpuid call
cpuid_feat1_p equ 0x60e ; word, EDX after cpuid call with EAX=1 arg
cpuid_feat2_p equ 0x610 ; word, ECX after cpuid call with EAX=1 arg

;;;  FIXME: we've lost a byte at 0x612 here !!!
video_screen_width_p equ 0x613 ; BYTE, max COLUMNS of a screen
video_cursor_column_p equ 0x614 ; BYTE, x position on a screen
video_cursor_row_p equ 0x615 ; BYTE, y position on a screen
video_mem_offset equ 0x616   ; WORD, cursor offset in a video mem


;;; TODO: place tmpGDT BEFORE e820 maps, because GDT is of known and fixed size
;;; TODO: is there a limit of entities in e820 table?
e820_table_p equ 0xC00 ; himem maps

;;; error code
err_boot0_load_failed equ 0x0101
err_boot0_bad_checksum equ 0x0102
err_boot0_boo1more_failed equ 0x0103
;
err_boot1_lowmem_err equ 0x0201
err_boot1_int15_failed equ 0x0202
err_boot1_e820_failed equ 0x0203

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

    ; save the boot disk pointer in DL
    push dx

    ; set video mode, assuming AH = 0
    mov AL, 0x03 ;  (80x25, color)
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
    call crlf

load_boot1:
.reset_drive:
    ; TODO: should I set ah=0x0d if we boot from a hard drive?
    mov ah, 0x00
    int 0x13

    mov	si, 3			; for i < 3, do:
.read:
    ; read next blocks from a boot disk into the memory
    mov ah, 0x02    ; read command
    mov al, 1+KERNEL_BLOCK_SZ    ; read 1 sector with boot1, and rest of the kernel blocks
    mov cx, 0x02 ; read SECOND block from disk, the first one is THIS one
    ; dx already contains the drive number we've boot from

    ; ES:BX = pointer to buffer
    mov bx, boot1 ; boot0+512 must be equal to boot1 addr
    int 0x13;
	jnc	.verify			;Success

    dec si
	jnz	.read

.load_err:
    mov ax, err_boot0_load_failed
    jmp print_err
.verify_err:
    mov ax, err_boot0_bad_checksum
    jmp print_err

.verify:
    cmp word[boot1_signature], 0xDEAD
    jne .verify_err

load_himem:
;;;  XXX: it will be a problem when kernel exceeds 128k
    mov si, start32  ; src pointer

    ; es:di is a dest pointer to the 1mb linear address
    mov di, 0x100
    mov ax, 0xfff0
    mov es, ax

.loop:
    mov ax, [si]
    mov [es:di], ax

    inc di
    inc si
    cmp si, start32 + KERNEL_BLOCK_SZ * 512
    jl .loop

.boot1_ready:
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
   ret

; assume error in AX:
;    AH - sub-system ID, references a part of a boot process
;    AL - the error code itself
print_err:
    push ax

    mov si, boot0data.str_err
    call print_string

    pop ax
    call print_word
    jmp $ ; hang forever


detect_cursor_pos:
    xor ax, ax
    ;;; TODO: move the whole word at once?
    mov al, byte[0x450]
    mov byte[video_cursor_column_p], al

    mov al, byte[0x451]
    mov byte[video_cursor_row_p], al

    mov al, byte[0x44A]
    imul ax, 2 ; each char takes 2 bytes in memory:
    ; the character value itself, and its color
    mov byte[video_screen_width_p], al

    ;;;  XXX: dirty experiments, cleanup needed.
    xor  ecx, ecx
    mov cl, byte[video_cursor_row_p]
    imul cx, word[video_screen_width_p]

    xor ax, ax
    mov al, byte[video_cursor_column_p]
    add cx, ax

    mov word[video_mem_offset], cx
    ; ^ now CX contains a valid memory *OFFSET* in video buffer
    ret



; boot0 data,
; note that printing \r \n (10, 13) via bios procedures
; corretly move the cursor on a next line, so you don;t have to advance
; the cursor position by hand, which is nice.
boot0data:
    .str_hello db 'boot0 started', 10, 13 , 0
    .str_bootdrive db 'boot drive: ', 0
    .str_err db 'ERR: ', 0
    .str_found db 'goto boot1', 10, 13, 0
    .tab_hextoc db '0123456789ABCDEF'

; boot1 data
boot1data:
    .var_buf_below_mb	db 0
    .var_buf_above_mb	db 0
    .str_hello db 'boot1 started', 10, 13, 0
    .str_lowmem db 'low mem: ', 0
    .str_a20_state db 'a20: ', 0
    .str_e820_regions db 'e820 reg: ', 0




times 510 - ($ - $$) db 0	;fill the rest of sector with 0
boot0_signature dw 0xAA55			; add boot signature at the end of bootloader

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
    mov ax, err_boot1_lowmem_err
    jmp print_err

.lowmem_print:
    ; save AL in the handover area
    mov [lowmem_p], ax
    call print_word
    mov al, 'k'
    call print_chr
    call crlf

.set_a20:
    ; enable A20 line
    in al, 0x92
    or al, 2
    out 0x92, al

    mov si, boot1data.str_a20_state
    call print_string

    call get_a20_state
    push ax
    add ax, '0'
    call print_chr
    call crlf
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
	jmp .print_himem
.failed:
    mov ax, err_boot1_lowmem_err
    jmp print_err ; dead end

.print_himem:
    mov si, boot1data.str_e820_regions
    call print_string

    ; here we believe there are at least some himem to use,
    ; let's go load code32 right at the 1mb mark.

    mov ax, bp
    call print_word
    call crlf

; get system configuration parameters
; https://stanislavs.org/helppc/int_15-c0.html
do_int15h:
    mov ah, 0xc0
    int 0x15
    jnc .save

    mov ax, err_boot1_int15_failed
    jmp print_err

.save:
    mov [int15_c0_table_p], es
    mov [int15_c0_table_p+2], bx


;;; TODO: more to dig here https://wiki.osdev.org/Detecting_CPU_Topology_(80x86)
cpuid:
.vendor:
    mov eax, 0
    cpuid
    ; copy results to the handover area
    mov [cpuid1_p], ebx
    mov [cpuid2_p], edx
    mov [cpuid3_p], ecx

.features:
    xor ecx, ecx
    xor edx, edx
    mov eax, 1
    cpuid

    ; copy results to the handover area
    mov [cpuid_feat1_p], edx
    mov [cpuid_feat2_p], ecx
    jmp setup_gdt

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
	mov byte [boot1data.var_buf_below_mb], al
	mov al, [es:di]
	mov byte [boot1data.var_buf_above_mb], al

	mov ah, 1						;	check byte [0x00100500] == byte [0x0500]
	mov byte [ds:si], 0
	mov byte [es:di], 1
	mov al, [ds:si]
	cmp al, [es:di]
	jne .exit
	dec ah
.exit:
	mov al, [boot1data.var_buf_below_mb]
	mov [ds:si], al
	mov al, [boot1data.var_buf_above_mb]
	mov [es:di], al
	shr ax, 8
	pop es
	pop ds
	pop di
	pop si
	popf
	ret

setup_gdt:
    xor ax, ax
    mov ds, ax
    lgdt [gdt.desc]

setup_pmode:
    mov eax, cr0
    or eax, 1
    mov cr0, eax

    call detect_cursor_pos

.dig_here:
    ;;;  XXX: looks like we've lost a return address on a stack after the int10
    ;;;  XXX: so it'd better to add pusha/popa here and here.
    ;mov ax, word[video_mem_offset]
   ; call print_word
   ; call crlf

    jmp code32:reload_CS


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

    xor ecx, ecx
    mov ecx, [video_mem_offset]
    add ecx, 0xB8000

    mov esi, .str32_hello
.hello32:
    mov al, byte[esi]
    or al, al
    jz .done

    mov byte[ecx], al
    inc ecx
    mov byte[ecx], 0x02
    inc ecx

    inc esi ; i++
    jmp .hello32

.done:
    hlt


.str32_hello db 'pmode: hey, in 32-bit mode now', 0


times 510 - ($ - boot1) db 0 ;; padding
boot1_signature dw 0xDEAD

;
; ----------------------------------------------------
; stub32 starts here
; ----------------------------------------------------
;

start32:
; mov eax, 0xB8000
; mov byte[eax], '@'
; inc eax
; mov byte[eax], 0x1b
;jmp $

times 510 - ($ - start32) db 0 ;; padding
start32_signature dw 0xCafe

; vim: filetype=nasm

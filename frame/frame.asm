


.model tiny
.code
.386
locals 
org 100h



.videoSegPtr	equ 0B800h
;------------------------------------------------
; multiplicate register to 80d (50h)
;------------------------------------------------
; Enter: reg = register name
;------------------------------------------------
MUL50 		macro reg
		push cx
		shl cx, 4
		add reg, cx
		shl cx, 2
		add reg, cx
		pop cx
		endm
;------------------------------------------------


start:
		mov ax, .videoSegPtr
		mov es, ax
	
		call parse_string		; (x, y)  --> bx (height, lengrh) --> dx, style --> cx, color --> ax, di --> message
		push si
		
		push dx				; 
		shr dx, 1			; length /= 2 and height /= 2
		sub bl, dh			; bl (y) -= height/2 and ->
		sub bh, dl  			; bh (x) -= length/2
		pop dx				;
	
		lea si, [frame_styles]
		cmp cx, 10d
		jne @@end_change
		xor cx, cx
		mov cl, [user_num]
@@end_change:
		call draw_frame
		pop si
		call print_frame_msg

		mov ax, 4C00h
		int 21h



;------------------------------------------------
; Gets font color offset 
;------------------------------------------------
; Enter: 	dl = frame length
; Expects:	(none)
; Destroys:	(none)
; Returns:	bx = color offset
;------------------------------------------------
get_clr_offset  proc
		push dx		
		push cx				
		xor dh, dh			
		shr dl, 1			
		shr cx, 1
		add bl, dl
		sub bl, 2			; offset -= two frame elements
		sub bx, cx
		pop cx
		pop dx
		ret 
		endp
;------------------------------------------------


;------------------------------------------------
; Parsing message and calculate message length
; and fill font colot buffer
;------------------------------------------------
; Expects:	msg_buffer = byte array for message
;		msg_len    = word 
;		font_color = byte array for 
;		each message color
;
;------------------------------------------------
parse_message	proc
		push di
		push ax
		push bx

		push si				;
		pop bx				;
		sub bx, 81h			; calculate message length with 
		mov cl, byte ptr ds:[80h]	; color change symbols
		sub cx, bx			;
		mov [msg_len], cx		; 

		push ds
		pop es
		lea di, [msg_buffer]
		lea bx, [font_color]
		
		push cx
		push si
@@L1:
		cmp byte ptr [si], '$'
		jne @@skip
		sub cx, 03h			; skip change color symbols
		sub [msg_len], 03h		; leght -= change color symbol
		add si, 03h
@@skip:
		movsb
		loop @@L1
		pop si


		mov cx, [msg_len]
	
		push dx		
		push cx				
		xor dh, dh			
		shr dl, 1			
		shr cx, 1
		add bl, dl
		sub bl, 2
		sub bx, cx
		pop cx
		pop dx

		pop cx
@@L2:
		cmp byte ptr [si], '$'
		jne @@no_color
		sub cx, 03h
		call get_color
@@no_color:
		mov [bx], ah
		inc bx
		inc si
		loop @@L2

		pop bx
		pop ax
		pop di
		ret 
		endp
;------------------------------------------------


;------------------------------------------------
; show shifting message
;------------------------------------------------
; Enter:	(none)
; Expects:	si = message pointer
; Destroys:	ax, bx, cx, dx
;------------------------------------------------
print_frame_msg	proc
		;shl ax, 8			; move color code from ax to ah (0004 --> 0400)
		xor cx, cx
		
		call parse_message
		lea si, [msg_buffer]
		
		push dx
		mov ax, 1130h			; 
		mov bx, 0600h			;
		int 10h
		pop dx				; bios interrupt, that returns pointer to code generator

		push di
		lea di, [buffer]		; pointer to symbol buffer
		mov cx, [msg_len]		; cx = message lenght

		call fill_buffer		
		pop di

		shr dh, 1
		mov cl, dh
@@div_loop:
		sub di, 80*2
		loop @@div_loop			; di = pointer to middle of the frame 

		add di, 2
			
		
		mov cx, [msg_len]
		shr cx, 1
		xor dh, dh
		sub dl, 2			; \
		mov [msg_len], dx		; / message lenght = frame lenght - 2 


		shr dx, 1			; \
		add dx, [msg_len]		;  | dx = 1.5 * (message length)
		sub dx, cx			; /

		mov cx, dx
		shl cx, 3			; cx = dx * 8 (8 shift per symbol)
		call show_message
		ret
		endp
;------------------------------------------------

;------------------------------------------------
; scanf ("%x")
;------------------------------------------------
; Enter: 	si = pointer to byte string
; Returns: 	ah = number of color 	
;------------------------------------------------
get_color 	proc
		push bx
		push cx
		xor ah, ah
		inc si	
		
		mov cx, 2
		
		jmp @@start

@@hex_symbol:
		mov bl, byte ptr [si]
		sub bl, 'A'- 0Ah
		add ah, bl
		jmp @@end_convert

@@start:
		shl ah, 4
		cmp byte ptr [si], 'A'
		ja @@hex_symbol
		
		mov bl, byte ptr [si]
		sub bl, '0'
		add ah, bl
@@end_convert:

		inc si
		loop @@start
		
		pop cx
		pop bx
		ret 
		endp
;------------------------------------------------


;------------------------------------------------
; Convert string value to number 
;------------------------------------------------
; Expects: 	si = message pointer
; Returns:	ax = number
;------------------------------------------------
scan_number	proc
		push bx
		xor ax, ax
		;cmp byte ptr [si], ' '
		;je @@end_skip
;@@skip_spaces:
		inc si
		;cmp byte ptr [si], ' '
		;je @@skip_spaces
;@@end_skip:

@@convert:
		mov bx, ax     			 ;ax * 10
		shl bx, 1
		mov ax, bx
		shl bx, 2
		add ax, bx

		xor bx, bx
		mov bl, [si]
		cmp bl, ':'			; if symbol ascii code > '9'
		jb @@end_error
		lea dx, [convert_error]		; print error message
		mov ah, 09h
		int 21h
@@end_error:
		sub bl, '0'			; convert ascii code to decimal digit
		inc si
		add ax, bx
		cmp byte ptr [si], ' '
		jne @@convert
		
		;pop bx
		;cmp bl, '-'			; add '-' if number is negative
		;jne end_proc			; if (negative)
		;neg ax				; 	number = -number
end_proc:
		pop bx
		ret
		endp
;------------------------------------------------



scan_style 	proc
		push ax
		push es
		push cx
		push ds
		pop es
		inc si

		mov cx, 09h
@@L1:
		cmp byte ptr [si], "\"   ;"
		jne @@end_slash
		call get_color
		shr ax, 08h
		stosb
		jmp @@end_L1
@@end_slash:
		movsb
@@end_L1:
		loop @@L1
		pop cx
		pop es
		pop ax
		ret 
		endp

;------------------------------------------------
; parses strig and fills regs
;------------------------------------------------
; Returns: 	ax, bx, cx, dx
;------------------------------------------------
parse_string 	proc
		push 81h			; psp command line string start adress
		pop si

		call scan_number
		mov dh, al			; dh = frame height
		call scan_number
		mov dl, al			; dl = frame length
		call scan_number
		mov bh, al 			; bh = x coordinate (left top frame angle)
		call scan_number
		mov bl, al			; bl = y coordinate (left top frame angle)
		call scan_number
		mov cx, ax			; cx = frame style
		
		cmp cx, 10d
		jne @@end
		push di
		lea di, [user_style]
		call scan_style
		pop di
@@end:
		call get_color		; ax = frame color

		ret
		endp
;------------------------------------------------


;------------------------------------------------
; Draw frame in videomemory
;------------------------------------------------
; Expects: 	frame_styles 
; Enter:	dh = frame height
;		dl = frame length
;		bh = left top x coordinate
;		bl = left top y coordinate
;		al = frame color
; Returns:	(none)
; Destroys: 	cx, si
;-----------------------------------------------
draw_frame	proc
		push bx
		push dx
		
		push cx
		xor cx, cx
		shl bx, 1
		mov cl, bh

		mov di, cx		;di += ah*2
		mov cl, bl
		shl cx, 4
		add di, cx
		shl cx, 2
		add di, cx

		sub dl, 2
		sub dh, 2

		pop cx
		mov bx, cx
		shl cl, 3
		add cx, bx
		add si, cx

		xor cx, cx
		mov cl, dl

		call draw_string
		add di, 80*2
		
@@next:
		call draw_string
		sub si, 3
		add di, 80*2
		dec dh
		cmp dh, 0
		jne @@next
		add si, 3
		call draw_string
		pop dx
		pop bx
		ret
		endp
;------------------------------------------------

;-----------------------------------------------
; Draw frame element (left_el middle_el*cx right_el)
;------------------------------------------------
; Enter: 	cx = count of repit moddle element
;	 	di = destination
;	 	si = source of frame element
; Destroys: 	ax
; Returns:	(none)
;------------------------------------------------
draw_string	proc
		push di
		push cx
		
		lodsb
		stosw
		
		lodsb
		rep stosw

		lodsb
		stosw

		pop cx
		pop di
		ret
		endp
;------------------------------------------------

include shift.asm



frame_styles 	db 0Dah, 0C4h, 0Bfh
		db 0B3h, 020h, 0B3h
		db 0C0h, 0C4h, 0D9h

		db 03h, 03h,  03h		; hearts
		db 03h, 020h, 03h
		db 03h, 03h,  03h
		
		db 0C9h, 0CDh, 0BBh		; double line
		db 0BAh, 020h, 0BAh
		db 0C8h, 0CDh, 0BCh

user_style 	db 9 dup (02h)
user_num 	db 03h

buffer 		db 16*32 dup (00h)   		; max message len 32
end_buffer:
msg_buffer	db 32    dup (00h)
font_color	db 32  	 dup (08h)
		db '<font debug>'
hex		db "0123456789ABCDEF"

shift_f db 0
		db '<flag debug>'

convert_error   db "can't convert symbol to number", 0Ah, 0Dh, '$' 
msg_len		dw ?
		db '<message length>'

delay 		dw 01h
end start

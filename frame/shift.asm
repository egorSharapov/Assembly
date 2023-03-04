

;------------------------------------------------
; fills buffer with message symbols
;------------------------------------------------
; Enter:	
; Expects:
; Destroys:	bx
; Returns:	(none)
;------------------------------------------------
fill_buffer proc
		push es
		push si
		push di
@@fill:
		push cx				; save cx for internal loop
		
		movzx bx, [si]			; bh = 0, bl = [si]
		shl bx, 4
		add bx, bp
		mov cx, 16			; bytes per symbol
@@next:
		mov al, es:[bx]
		mov ds:[di], al
		inc bx
		inc di
		loop @@next

		pop cx
		inc si
		loop @@fill
		pop di
		pop si
		pop es 
		ret
endp 
;------------------------------------------------


;------------------------------------------------
; horizontaly moves string by 1 pixel
;------------------------------------------------
; Expects: 	si = string pointer
; Returns:	(none)
;------------------------------------------------
shift_string 	proc
		push di
		push si
		xor di, di

		shl bx, 4
@@mvstr:

		mov ah, byte ptr [buffer + bx + di]  ; + 5*16 + 1 msg_len*16 - 15
		sahf
		rcr byte ptr [buffer+di], 1
		setc  al
		mov [shift_f], al
	
		lea si, buffer
		add si, di
		
		push si
		lea cx, [end_buffer]
		add cx, di
		sub cx, 16
@@movbyte:
		add si, 10h
		mov ah, byte ptr [shift_f]
		sahf
		rcr byte ptr [si], 1
		setc al
		mov [shift_f], al
		
		cmp si, cx 
		jne @@movbyte

		pop si
		
		inc di
		cmp di, 16
		jne @@mvstr
		
		pop si
		pop di
		ret
endp
;------------------------------------------------



;------------------------------------------------
; prints shifting string to a videomemory
;------------------------------------------------
; Enters:	cx = shift count
; Expects: 	di = destination pointer
; Destroys:	bx
;------------------------------------------------
show_message 	proc
@@shift_print:
		push cx				;>>>>>>---------+
		lea bx, [font_color]
		push bx				; >>>>--+	|
						;	|	|
		mov bx, [msg_len]		;	|	|
		call shift_string		;	|	|
		call load_font			;	|	|
						;	|	|
		push 0b800h			;	|	|
		pop es				;	|	|
		pop bx				; <<<<--+	|
						;		|
		push di      			; >>>>>-+	|
		mov al, 80h			;	|	|
@@:	
		mov ah, [bx]			;	|	|
		stosw				;	|	|
		inc al				;   	|	|
		inc bx
		loop @@				;   	|	|
		
		call is_skip			;   	|	|
		pop di				; <<<<<-+	|
		pop cx				;<<<<<----------+
		
		loop @@shift_print
		ret
endp
;------------------------------------------------


is_skip		proc
		push ax
		mov ah, 0Bh
		int 21h
		cmp al, 0ffh
		je @@end
		mov ah, 86h
		mov cx, 1d
		int 15h

@@end:
		pop ax
		ret
		endp


;------------------------------------------------
; loads font from user buffer to 
;			     80h symbol generator
;------------------------------------------------
; Expects: msg_len = load message length
; 	   buffer  = uset font buffer
;-----------------------------------------------
load_font 	proc
		mov ax, 1100h
		mov cx, [msg_len]
		mov dx, 80h
		xor bx, bx
		mov bh, 16
		push ds
		pop es
		lea bp, buffer
		int 10h

		ret
endp
;------------------------------------------------


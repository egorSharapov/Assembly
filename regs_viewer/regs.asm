.286
.model tiny
.code
locals
org 100h


Start:
jmp init_handler

CONVERT 	macro res
		mov ax, res
		call convert_hex            	; update image
		add di, columns
endm


;------------------------------------------------
; Convert ax into hex and write to buffer
;------------------------------------------------
; Enters: 	di = buffer
; Expects:	hex = '0123456789abcdf'
; Destroys:	(none)
;------------------------------------------------
convert_hex	proc
		push ax bx cx dx di si ds es

		push cs
		push cs
		pop ds
		pop es

	
		xor dx, dx
		xor bx, bx	
@@convert:
		mov cx, 4d
hex_loop:
		inc dx
		shl bx, 1
		shl ax, 1
		jnc add0
		add bx, 1
add0:
		loop hex_loop
		
		mov si, offset hex
		add si, bx
		movsb
		mov byte ptr es:[di], green
		inc di
		xor bx, bx
		cmp dx, 16
		jne @@convert
		
		pop es
		pop ds
		pop si
		pop di
		pop dx
		pop cx
		pop bx
		pop ax
		ret
endp
;------------------------------------------------


;------------------------------------------------
; Compare video and image buffers. If video
; buffer has changed, write changes to saved
;------------------------------------------------
; Enters:	si = image buffer
;		bx = coordinates in videomem
; Expects:	saved_buffer
; Destroys: 	(none)
;------------------------------------------------
cmp_buffers	proc
		push ax bx cx di

;               convert coordinates to adress
		xor cx, cx
		shl bx, 1
		mov cl, bh

		mov di, cx		;di += bh*2
		mov cl, bl
		shl cx, 4
		add di, cx 
		shl cx, 2
		add di, cx


		mov dx, columns
		mov cx, columns*lines/2
		xor bx, bx
		xor ax, ax
@@compare:
		mov ax, cs:[si + bx] 			; ax = image[si]
		cmp ax, es:[di + bx]			; if ax == video[si]
		je @@end_update				; update saved
		mov ax, es:[di + bx]
		mov cs:[saved_buffer + bx], ax
@@end_update:
		
		add bx, 02h

		cmp bx, dx				; if bx > buffer lenght
		jne @@end_newline 			; di += new_line adress
		add dx, columns
		add di, 80*2
		sub di, columns
@@end_newline:
		loop @@compare

		pop di cx bx ax
		ret
		endp
;------------------------------------------------


;------------------------------------------------
; Copy buffer from image to videomemory
;-----------------------------------------------
; Enters: 	bx = coordinates in videomem 
; 		si = buffer to show
; 		fix size lines*columns
; Expects: 	(none)
; Destroys: 	(none)
;------------------------------------------------
show_buffer	proc
		push bx cx es di ds

		xor cx, cx
		shl bx, 1
		mov cl, bh

		mov di, cx		;di += ah*2
		mov cl, bl
		shl cx, 4
		add di, cx 
		shl cx, 2
		add di, cx

		push cs
		pop ds 

		mov bx, 0b800h
		mov es, bx
	
		mov cx, lines
@@columns:
		push cx
		push di
		mov cx, columns         	    ; old: mov cx 08h movsw
		rep movsb
		pop di
		add di, 80*2		  	; new line
		pop cx
		loop @@columns
		pop ds di es cx bx
		ret
		endp


;------------------------------------------------


; compare buffers ( video and image)
; write to saved, if video has changed
; update image
; show image
; in cleaner show saved
; end
;------------------------------------------------
New08 		proc
		mov cs:[bx_save], bx
		mov cs:[si_save], si
		mov cs:[di_save], di
		mov cs:[bp_save], bp
		mov cs:[sp_save], sp
		mov cs:[ds_save], ds
		mov cs:[es_save], es
		mov cs:[ss_save], ss
		mov cs:[cs_save], cs
	
		pushf				; save flags
		db 09ah 			; call far
Old08Ofs 	dw 0
Old08Seg 	dw 0
		push ax bx es
	
		mov bx, 0b800h
		mov es, bx
		mov bx, cs:[video_adress]

		cmp cs:[delay], 3d
		je far_sleep
		jmp sleep
far_sleep:
		cmp cs:[mode], 01h		; show mode
		je update
		cmp cs:[mode], 02h		; dont update
		jne far_clean
		jmp clean
far_clean:
		jmp sleep
update:
		lea si, cs:[image_buffer]
		call cmp_buffers

		lea di, cs:[image_buffer+26]
		call convert_hex            	; update image
		add di, columns

		CONVERT cs:[bx_save]
		CONVERT cx
		CONVERT dx
		CONVERT cs:[si_save]
		CONVERT cs:[di_save]
		CONVERT cs:[bp_save]
		CONVERT cs:[sp_save]
		CONVERT cs:[ds_save]
		CONVERT cs:[es_save]
		CONVERT cs:[ss_save]
		CONVERT cs:[cs_save]
		lea si, cs:[image_buffer]
		call show_buffer
		jmp sleep
clean:
		lea si, cs:[saved_buffer]
		call show_buffer
		mov cs:[mode], 00h
sleep:	
		inc cs:[delay]
		cmp cs:[delay], 04h
		jne not_reset
		mov cs:[delay], 00h
not_reset:
		pop es bx ax
		
		iret
		endp
;------------------------------------------------

;------------------------------------------------
; Keyboard handler
;------------------------------------------------
New09 		proc
		push ax
		in al, 60h
		cmp al, 0Dh
		je @@pressed
		jmp @@sleep
@@pressed:
		add cs:[mode], 01h

		in al, 61h
		or al, 80h
		out 61h, al
		and al, not 80h
		out 61h, al
		
		mov al, 20h
		out 20h, al

		pop ax
		iret
@@sleep:
		pop ax
		db 0eah 			; jmp far
Old09Ofs 	dw 0h
Old09Seg 	dw 0h

		endp
;------------------------------------------------


;------------------------------------------------
; Old multiplex handler
;------------------------------------------------
New02		proc

		cmp ax, 0ff00h
		jne @@already_inst
		mov ax, 00ffh
		iret
@@already_inst:
		db 0eah 			; jmp far
Old02Ofs 	dw 0h
Old02Seg 	dw 0h
		endp 
;------------------------------------------------

;================================================
; Resident data
lines		equ 14
columns		equ 18
green		equ 0A2h

mode		db 00h
bx_save 	dw 0BEDAh
si_save		dw 0BEDAh
di_save		dw 0BEDAh
bp_save		dw 0BEDAh		
sp_save		dw 0BEDAh
ds_save 	dw 0BEDAh
es_save 	dw 0BEDAh
ss_save 	dw 0BEDAh
cs_save 	dw 0BEDAh
image_buffer	dw columns*lines/2 dup (0020h)	
saved_buffer	dw lines*(2+1+4+2) dup (4050h)
video_adress	dw 4001h
delay 		db 00h
hex		db '0123456789ABCDEF'
resident_end:

;================================================

init_handler: 
		mov ax, 1003h
		mov bx, 0h
		int 10h
		mov bx, [video_adress]
		mov dh, lines
		mov dl, columns/2
		lea si, [frame_styles]
		lea di, [image_buffer]
		mov ah, green 
		push ds
		pop es
		call draw_frame

		mov ah, green
		lea si, [signatures]
		lea di, [image_buffer+columns+2]
		mov dh, lines-2
		mov dl, 02h
		call add_signatures


		mov ax, 0ff00h
		int 02fh
		cmp ax, 00ffh
		jne not_installed
		mov ah, 09h
		lea dx, [warning]
		int 21h
		mov ax, 4C00h
		int 21h
not_installed:
						
		xor bx, bx
		mov es, bx
		mov bx, 2fh*4
		mov ax, es:[bx]
		mov [Old02Ofs], ax
		mov ax, es:[bx+02]
		mov [Old02Seg], ax
		cli
		mov es:[bx], offset New02
		mov ax, cs
		mov es:[bx+02], ax
		sti

				
		xor bx, bx			; handle timer interrupt
		mov es, bx
		mov bx, 8*4
		mov ax, es:[bx]
		mov [Old08Ofs], ax
		mov ax, es:[bx+02]
		mov [Old08Seg], ax
		cli
		mov es:[bx], offset New08
		mov ax, cs
		mov es:[bx+02], ax
		sti

		
		xor bx, bx			; handle keyboard interrupt
		mov es, bx
		mov bx, 9*4
		mov ax, es:[bx]
		mov [Old09Ofs], ax
		mov ax, es:[bx+02]
		mov [Old09Seg], ax
		cli
		mov es:[bx], offset New09
		mov ax, cs
		mov es:[bx+02], ax
		sti
		mov ax, 3100h
		lea dx, resident_end
		shr dx, 4
		inc dx
		int 21h

;------------------------------------------------
; Adds register names in image buffer
;------------------------------------------------
add_signatures	proc
		xor cx, cx
@@next:
		mov cl, dl
		push di
@@line:
		lodsb
		stosw
		loop @@line
		pop di
		add di, columns
		dec dh
		cmp dh, 0
		jne @@next
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

		sub dl, 2
		sub dh, 2

		xor cx, cx
		mov cl, dl

		call draw_string
@@next:
		call draw_string
		sub si, 3
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
		push cx
		
		lodsb
		stosw
	
		lodsb
		rep stosw

		lodsb
		stosw

		pop cx
		ret
		endp
;------------------------------------------------

;================================================
; Init handlerda data
signatures	db 'ax', 'bx', 'cx', 'dxsidibpspdsescsss'
warning		db 'programm is already installed$'
frame_styles 	db 0Dah, 0C4h, 0Bfh
		db 0B3h, 020h, 0B3h
		db 0C0h, 0C4h, 0D9h
;================================================
end Start

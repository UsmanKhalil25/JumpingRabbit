[org 0x0100]
jmp start
;--------------------------
screenBuffer: times 3000 dw 0	;132 * 14
rows: dw 43
columns: dw 132
scoreWord: db 'Score: ',0
gameoverWord:db 'Game Over',0
scoreNumber: dw 0
tileMovementVar: dw 20
movingTileRow: dw 30
heightOfTile: dw 0
rabbitRow: dw 39
rabbitCol: dw 63
oldisr: dd 0
carrotRow: dw 32
carrotCol: dw 64
timerCount: dw 0
isRabbitOnBlueTile: dw 0
blueTileCount: dw 0
gameTitle: db 'Jumping Rabbit',0
member1: db 'Usman Khalil 22L-6873',0
member2: db 'Farrukh Awan 22L-6701',0
pressEnterText: db 'Press enter to start the game',0
gameStarted: dw 0
gamePaused: dw 0
loadingText:  db 'Loading',0
pauseGameText1: db 'Do you want to exit?',0
pauseGameText2: db '   Yes[y]  No[n]    ',0


;--------------------------
cls:
	push es
	push ax
	push di
	push cx

	mov ax, 0xb800
	mov es, ax					; point es to video base
	mov di, 0					; point di to top left column
	mov ax,[rows]
	mul word[columns]
	mov cx,ax
	
clear:	
	mov word [es:di], 0x0720	; clear next char on screen
	add di, 2					; move to next screen location
	loop clear
	
	pop cx
	pop di
	pop ax
	pop es
	
	ret
;--------------------------
delay1:
	push cx
	mov cx,0xFFFF
loopdelay11: loop loopdelay11
	mov cx,0xFFFF
loopdelay12: loop loopdelay12
	mov cx,0xFFFF
loopdelay13: loop loopdelay13
	mov cx,0xFFFF
loopdelay14: loop loopdelay14
	pop cx
	ret
;-------------------------
delay2:
	push cx
	mov cx,0xFFFF
loopdelay21: loop loopdelay21
	pop cx
	ret
;-------------------------

printstr:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	
	push ds
	pop es
	mov di,[bp+4]
	mov cx,0xffff
	xor al,al
	repne scasb
	mov ax,0xffff
	sub ax,cx
	dec ax
	jz exit

	mov cx,ax
	mov ax,0xb800
	mov es,ax
	mov al,[columns]
	mul byte[bp+8]
	add ax,[bp+10]
	shl ax,1
	mov di,ax
	mov si,[bp+4]
	mov ah,[bp+6]
	cld 
nextchar:
	lodsb
	stosw
	loop nextchar
exit:
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 8
;--------------------------
printNum:
	push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax,0xb800
	mov es,ax
	mov ax,[bp+4]
	mov bx,10
	mov cx,0

nextDigit:
	xor dx,dx
	div bx
	add dl,0x30
	push dx
	inc cx
	cmp ax,0
	jnz nextDigit

	mov al,[columns]
	mul byte[bp+8]
	add ax,[bp+10]
	shl ax,1
	mov di,ax

nextPosition:
	pop dx
	mov dh,[bp+6]
	mov [es:di],dx
	add di,2
	loop nextPosition


	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 8
;--------------------------
horizontalLine:
	push bp 
	mov bp,sp
	push ax
	push es
	push di
	push cx
	
	mov ax,0xB800
	mov es,ax
	
	mov ax, 132
	mul word [bp + 8]   ; starting row
	add ax, word [bp + 6]  ; starting col
	shl ax, 1

	mov di, ax
	mov ax,[bp+4]
	mov cx,word[bp+10]

drawHorizontalLine:
	mov word[es:di],ax
	add di,2
	loop drawHorizontalLine
	
	pop cx
	pop di
	pop es
	pop ax
	pop bp
	ret 8

;--------------------------
divideScreen:
	push ax
	push dx
	push bx
	
	mov ax,[rows]	;moving the number of rows in ax
	mov bx,3		; moving 3 into bx
	xor dx,dx		; clearing dx before dividing
	div bx
	mov bx,ax		; bx contains 1/3 length of screen
	; upper 1/3 part
	mov ax,word[columns]	;length of line
	push ax
	mov ax,bx		;14th row
	push ax
	mov ax,0	;starting col
	push ax
	mov ah,0x07
	mov al,'_'	;character to print
	push ax
	call horizontalLine

	mov ax,word[columns]	;length of line
	push ax
	mov ax,bx
	add ax,ax			;14+14 = 28th row
	dec ax				; to acquire more space for the bottom game
	push ax
	mov ax,0	;starting col
	push ax
	mov ah,0x07
	mov al,'_'	;character to print
	push ax
	call horizontalLine
	
	pop bx
	pop dx
	pop ax
	ret
		
;------------------------
sky:
	push bp
	mov bp,sp
	push ax
	push dx
	push bx
	push cx
	
	mov ax,[rows]	;moving the number of rows in ax
	mov bx,3		; moving 3 into bx
	xor dx,dx		; clearing dx before dividing
	div bx
	mov bx,ax		; bx contains 1/3 length of screen
	mov cx,ax
	inc cx		; to run the loop till 0th row
	mov bx,[bp+4]

fillSky:
	mov ax,word[columns]	;length of line
	push ax
	mov ax,bx		;14th row
	push ax
	mov ax,0	;starting col
	push ax
	mov ah,0x30
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	inc bx
	loop fillSky

	pop cx
	pop bx
	pop dx
	pop ax
	pop bp
	ret 2
	
;-------------------------
window:
	push bp 
	mov bp,sp
	push ax
	push es
	push di
	push cx
	
	mov ax,0xB800
	mov es,ax
	
	mov ax, 132
	mul word [bp + 8]   ; starting row
	add ax, word [bp + 6]  ; starting col
	shl ax, 1

	mov di, ax
	mov ax,[bp+4]
	mov cx,word[bp+10]

drawWindow:
	mov word[es:di],ax
	add di,2
	mov word[es:di],ax
	add di,2
	mov word[es:di],ax
	add di,4
	loop drawWindow
	
	pop cx
	pop di
	pop es
	pop ax
	pop bp
	ret 8

;-------------------------
largeBuilding:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	mov cx,14	;Height of the building
	mov bx,[bp+4]	; starting row of the building
	
largeBuildingDraw:
	mov ax,14	;length of line (Width of the building)
	push ax
	mov ax,bx	;starting row
	push ax
	mov ax,word[bp+6]	;starting col
	push ax
	mov ah,0x10
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	dec bx
	loop largeBuildingDraw
	
	
	mov bx,[bp+4]	; starting row of the building
	mov cx,[bp+4]
	sub cx,8
largeBuildingWindows:
	mov ax,3	;length of line (Width of the building)
	push ax
	mov ax,bx	;starting row
	sub ax,2	; starting point of the windows lastrow-2
	push ax
	mov ax,word[bp+6]	;starting col
	add ax,2	; starting point of the windows column-2
	push ax
	mov ah,0x70  ; window color (blue)
    mov al,0x20  ;character to print
	push ax
	call window
	sub bx,2
	loop largeBuildingWindows
	
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
;-------------------------
mediumBuilding:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	mov cx,12	;Height of the building
	mov bx,[bp+4]	; starting row of the building
mediumBuildingDraw:
	mov ax,12	;length of line (Width of the building)
	push ax
	mov ax,bx	;starting row
	push ax
	mov ax,word[bp+6]	;starting col
	push ax
	mov ah,0x10
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	dec bx
	loop mediumBuildingDraw
	
	
	mov bx,[bp+4]	; starting row of the building
	mov cx,[bp+4]
	sub cx,9
mediumBuildingWindows:
	mov ax,2	;length of line (Width of the building)
	push ax
	mov ax,bx	;starting row
	sub ax,2	; starting point of the windows lastrow-2
	push ax
	mov ax,word[bp+6]	;starting col
	add ax,2	; starting point of the windows column-2
	push ax
	mov ah,0x70  ; window color (yellow)
    mov al,0x20  ;character to print
	push ax
	call window
	sub bx,2
	loop mediumBuildingWindows
	
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
;-------------------------
smallBuilding:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	mov cx,10	;Height of the building
	mov bx,[bp+4]	; starting row of the building
smallBuildingDraw:
	mov ax,10	;length of line (Width of the building)
	push ax
	mov ax,bx	;starting row
	push ax
	mov ax,word[bp+6]	;starting col
	push ax
	mov ah,0x70
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	dec bx
	loop smallBuildingDraw
	
	
	mov bx,[bp+4]	; starting row of the building
	mov cx,[bp+4]
	sub cx,10
smallBuildingWindows:
	mov ax,2	;length of line (Width of the building)
	push ax
	mov ax,bx	;starting row
	sub ax,2	; starting point of the windows lastrow-2
	push ax
	mov ax,word[bp+6]	;starting col
	add ax,2	; starting point of the windows column-2
	push ax
	mov ah,0x10  ; window color (yellow)
    mov al,0x20  ;character to print
	push ax
	call window
	sub bx,2
	loop smallBuildingWindows
	
	
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4

;-------------------------
upperPortion:
	push ax
	
	mov ax,0
	push ax
	call sky
	
	mov ax,0	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call largeBuilding         
	
	mov ax,15	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call mediumBuilding
	
	mov ax,25	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call smallBuilding
	
	mov ax,34	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call mediumBuilding
	
	mov ax,47	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call smallBuilding
	
	mov ax,56	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call largeBuilding
	
	mov ax,70	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call mediumBuilding
	
	mov ax,80	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call smallBuilding
	
	mov ax,92	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call mediumBuilding
	
	mov ax,102	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call mediumBuilding
		
	mov ax,112	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call largeBuilding
	
	mov ax,122	;starting column
	push ax
	mov ax,14	;starting row
	push ax
	call smallBuilding
	
	
	pop ax
	ret
;-------------------------
roadStrip:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push si
	push dx
	
	mov bx,0	;storing the starting column in bx
	mov ax,[columns]	; calculating the number of times the loop will run 132/12 = 11
	mov si,12
	xor dx,dx
	div si
	mov cx,ax
	inc cx
drawRoadStrip:
	mov ax,[columns]	;length of line
	mov si,12
	xor dx,dx
	div si
	shr ax,1	;11/2 = 5
	push ax
	mov ax,[bp+4]	;starting row
	push ax
	mov ax,bx	;starting col
	push ax
	mov ah,0x07
	mov al,'_'	;character to print
	push ax
	call horizontalLine
	mov ax,[columns]	;132/12 = 11
	mov si,12
	xor dx,dx
	div si
	add bx,ax
	loop drawRoadStrip
	
	pop dx
	pop	si
	pop cx
	pop bx
	pop ax
	pop bp
	
	ret 2
;--------------------------
car:
	push bp
	mov bp,sp
	push ax
	push cx
	
	
	mov ax,14	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	push ax
	mov ax,[bp+6]	;starting col
	push ax
	mov ah,[bp+4]
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	
	mov ax,10	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	dec ax
	push ax
	mov ax,[bp+6]	;starting col
	inc ax
	inc ax
	push ax
	mov ah,[bp+4]
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	;head light
	mov ax,2	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	push ax
	mov ax,[bp+6]	;starting col
	sub ax,2
	push ax
	mov ah,0x70
	mov al,'o'	;character to print
	push ax
	call horizontalLine
	;back light
	mov ax,1	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	push ax
	mov ax,[bp+6]	;starting col
	add ax,13  
	push ax
	mov ah,0x70
	mov al,'o'	;character to print
	push ax
	call horizontalLine
	
	
	mov ax,1	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	push ax
	mov ax,[bp+6]	;starting col
	add ax,7
	push ax
	mov ah,[bp+4]
	mov al,'|'	;character to print
	push ax
	call horizontalLine
	
	mov ax,1	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	push ax
	mov ax,[bp+6]	;starting col
	sub ax,[columns]
	add ax,7
	push ax
	mov ah,[bp+4]
	mov al,'|'	;character to print
	push ax
	call horizontalLine
	; wind sheild
	mov ax,5	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	dec ax
	push ax
	mov ax,[bp+6]	;starting col
	add ax,2
	push ax
	mov ah,0x70
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	
	mov ax,1	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	push ax
	mov ax,[bp+6]	;starting col
	add ax,[columns]
	push ax
	mov ah,0x07
	mov al,'O'	;character to print
	push ax
	call horizontalLine
	
	mov ax,1	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	push ax
	mov ax,[bp+6]	;starting col
	add ax,[columns]
	add ax,2
	push ax
	mov ah,0x07
	mov al,'O'	;character to print
	push ax
	call horizontalLine
	;front tyre
	mov ax,1	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	push ax
	mov ax,[bp+6]	;starting col
	add ax,[columns]
	add ax,10
	push ax
	mov ah,0x07
	mov al,'O'	;character to print
	push ax
	call horizontalLine
	;rare tyre
	mov ax,1	;length of line
	push ax
	mov ax,[bp+8]	;starting row
	push ax
	mov ax,[bp+6]	;starting col
	add ax,[columns]
	add ax,12
	push ax
	mov ah,0x07
	mov al,'O'	;character to print
	push ax
	call horizontalLine
	
	
	
	pop cx
	pop ax
	pop bp
	ret 6
;--------------------------
tree:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	
	mov bx,[bp+6]	; starting row
	mov cx,3
	
drawTree:

	mov ax,1	;length of line
	push ax
	mov ax,bx	;starting row
	push ax
	mov ax,[bp+4]	;starting col
	push ax
	mov ah,0x60
	mov al,0x20	;character to print
	push ax
	call horizontalLine

	dec bx
	loop drawTree
	
	; leaves 1
	mov bx,[bp+6]	; starting row
	mov ax,7	;length of line
	push ax
	mov ax,bx	;starting row
	sub ax,3
	push ax
	mov ax,[bp+4]	;starting col
	sub ax,3
	push ax
	mov ah,0x20
	mov al,0x20	;character to print
	push ax
	call horizontalLine

	; leaves 2
	mov ax,5	;length of line
	push ax
	mov ax,bx	;starting row
	sub ax,4
	push ax
	mov ax,[bp+4]	;starting col
	sub ax,2
	push ax
	mov ah,0x20
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	
	; leaves 3
	mov ax,3	;length of line
	push ax
	mov ax,bx	;starting row
	sub ax,5
	push ax
	mov ax,[bp+4]	;starting col
	sub ax,1
	push ax
	mov ah,0x20
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	
	
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
	
;--------------------------
footPath:
	push bp
	mov bp,sp
	push ax
	push bx
	push dx
	push cx
	
	mov bx,[bp+4]
	mov cx,2
	
	; upper 1/3 part
drawFootPath:
	mov ax,word[columns]	;length of line
	push ax
	mov ax,bx		;15th and 16th row
	push ax
	mov ax,0	;starting col
	push ax
	mov ah,0x20
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	inc bx
	loop drawFootPath
	
	pop bp
	pop cx
	pop dx
	pop bx
	pop ax
	ret 2
;--------------------------
middlePortion:
	
	push ax
	push bx
	push cx
	push dx
	push si
	
	mov ax,[rows]	;moving the number of rows in ax
	mov bx,3		; moving 3 into bx
	xor dx,dx		; clearing dx before dividing
	div bx
	mov bx,ax		; bx contains 1/3 length of screen
	inc bx			; to move to the 15th row
	push bx			; starting row bp+4
	call footPath

	mov ax,[rows]
	mov si,3
	xor dx,dx
	div si		; screen is divided into 3 parts
	mov bx,ax	; storing result into bx
	shr ax,1	;to get half of one 1/3 section
	add bx,ax	; adding to reach the middle of middle section
	mov si,bx	; we dont want to destroy bx yet
	push si
	call roadStrip
	
	
	
	;lower car
	mov ax,bx
	add ax,3	
	push ax		; starting row	bp+8
	mov ax,word[columns]
	sub ax,28
	push ax		; starting column	bp+6
	mov ax,0x10	; color of car	bp+4
	push ax
	call car
	
	; upper car
	mov ax,bx
	sub ax,3	
	push ax		; starting row	bp+8
	mov ax,word[columns]
	sub ax,38
	push ax		; starting column	bp+6
	mov ax,0x40	; color of car	bp+4
	push ax
	call car


	mov ax,[columns]
	mov si,5
	mul si
	mov si,100
	xor dx,dx
	div si	; columns * 5 % 
	mov si,ax
	mov cx,ax	
	mov bx,10
	
loopTree:
    mov ax, 15     	; starting row
    push ax         ; bp + 6
    mov ax, bx      ; starting columns
    push ax         ; bp + 4
    call tree
    add bx, 22    ; Update bx for the next iteration
    loop loopTree
	
	
	
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
;-------------------------
rabbit:
	;bp+4 column
	;bp+6 rows
	push bp
	mov bp,sp
	push ax
	push es
	push di

	mov ax,0xb800
	mov es,ax
	mov al,132
	mul byte[bp+6]
	add ax,[bp+4]
	shl ax,1
	mov di,ax

	mov ah,0x30

	mov al,'('
	mov [es:di],ax
	add di,2
	mov al,'_'
	mov [es:di],ax
	add di,2
	mov al,'_'
	mov [es:di],ax
	add di,2
	mov al,'_'
	mov [es:di],ax
	add di,2
	mov al,')'
	mov [es:di],ax
	add di,2
	mov al,'o'
	mov [es:di],ax

	sub di,2
	sub di, 264	;132x2 = 264
	mov al,')'
	mov [es:di],ax

	sub di,2
	mov al,'^'
	mov [es:di],ax

	sub di,2
	mov al,'_'
	mov [es:di],ax

	sub di,2
	mov al,'^'
	mov [es:di],ax

	sub di,2
	mov al,'('
	mov [es:di],ax

	sub di,264

	mov [es:di],ax

	add di,2
	mov al,'\'
	mov [es:di],ax

	add di,2
	mov al,'_'
	mov [es:di],ax

	add di,2
	mov al,'/'
	mov [es:di],ax

	add di,2
	mov al,')'
	mov [es:di],ax


	pop di
	pop es
	pop ax
	pop bp
	ret 4
;-------------------------
removeRabbit:
	push ax
	mov ax,6	;length of line
	push ax
	mov ax,[cs:rabbitRow]		; row
	push ax
	mov ax,[cs:rabbitCol]	;starting col
	push ax
	mov ah,0x30
	mov al,0x20	;character to print
	push ax
	call horizontalLine

	mov ax,5	;length of line
	push ax
	mov ax,[cs:rabbitRow]		; row
	dec ax
	push ax
	mov ax,[cs:rabbitCol]	;starting col
	push ax
	mov ah,0x30
	mov al,0x20	;character to print
	push ax
	call horizontalLine

	mov ax,5	;length of line
	push ax
	mov ax,[cs:rabbitRow]		; row
	dec ax
	dec ax
	push ax
	mov ax,[cs:rabbitCol]	;starting col
	push ax
	mov ah,0x30
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	pop ax
	ret
;-------------------------
printScore:
	push ax

	mov ax,5	; starting column
	push ax
	mov ax,28	; starting row
	push ax	
	mov ax,0x30	; attribute
	push ax
	mov ax,scoreWord
	push ax
	call printstr
	mov ax,12	; starting column
	push ax
	mov ax,28	; starting row
	push ax	
	mov ax,0x30	; attribute
	push ax
	mov ax,[scoreNumber]
	push ax
	call printNum


	pop ax
	ret
;-------------------------
lowerPortion:
	push ax

	mov ax,28
	push ax
	call sky
	; to print the score
	
	call printScore
	; to print grass
	mov ax,[columns]	;length of line
	push ax
	mov ax,42		; row
	push ax
	mov ax,0	;starting col
	push ax
	mov ah,0x60
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	mov ax,[columns]	;length of line
	push ax
	mov ax,41		; row
	push ax
	mov ax,0	;starting col
	push ax
	mov ah,0x60
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	mov ax,[columns]	;length of line
	push ax
	mov ax,40		; row
	push ax
	mov ax,0	;starting col
	push ax
	mov ah,0x20
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	; to print tile
	mov ax,10	;length of line
	push ax
	mov ax,35		; row
	push ax
	mov ax,60	;starting col
	push ax
	mov ah,0x60
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	mov ax,10	;length of line
	push ax
	mov ax,30		; row
	push ax
	mov ax,60	;starting col
	push ax
	mov ah,0x60
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	; to print bunny
	mov ax,39		; row
	push ax
	mov ax,63	;starting col
	push ax
	call rabbit
	call spawnCarrot
	pop ax
	ret

;-------------------------
rabbitFallAnimation:
	push ax
	push cx

	mov cx,4
loopRabbitFall:
	call removeRabbit
	inc word[cs:rabbitRow]
	mov ax,[cs:rabbitRow]		; row
	push ax
	mov ax,[cs:rabbitCol]	;starting col
	push ax
	call rabbit
	call delay1
	call delay1
	loop loopRabbitFall

	pop cx
	pop ax
	ret
;-------------------------
gameoverScreen:
	push ax

	call cls
	mov ax,0
	push ax
	call sky
	mov ax,14
	push ax
	call sky
	mov ax,28
	push ax
	call sky

	mov ax,60	; starting column
	push ax
	mov ax,22	; starting row
	push ax	
	mov ax,0x31	; attribute
	push ax
	mov ax,scoreWord
	push ax
	call printstr
	
	mov ax,67	; starting column
	push ax
	mov ax,22	; starting row
	push ax	
	mov ax,0x31	; attribute
	push ax
	mov ax,[cs:scoreNumber]
	push ax
	call printNum

	mov ax,60	; starting column
	push ax
	mov ax,21	; starting row
	push ax	
	mov ax,0x31	; attribute
	push ax
	mov ax,gameoverWord
	push ax
	call printstr

	pop ax 
	ret
;-------------------------
gameover:
	push ax
	call rabbitFallAnimation
	call gameoverScreen

	mov word[cs:gameStarted],0
	
cli
	mov ax,[cs:oldisr]
	mov [es:9*4],ax
	mov ax,[cs:oldisr+2]
	mov [es:9*4+2],ax
sti

	pop ax
	ret 

;-------------------------
loadingScreen:
	push ax
	push bx
	push cx

	call cls

	mov ax,0
	push ax
	call sky
	mov ax,14
	push ax
	call sky
	mov ax,28
	push ax
	call sky

    mov ax,62	; starting column
	push ax
	mov ax,18	; starting row
	push ax	
	mov ax,0x31	; attribute
	push ax
	mov ax,loadingText
	push ax
	call printstr

    mov ax,40
	push ax
	mov ax, 20  ;starting row
	push ax
	mov ax,46	;starting col
	push ax
	mov ah,0x31
	mov al,'-'	;character to print
	push ax
	call horizontalLine

    mov ax,40
	push ax
	mov ax, 22  ;starting row
	push ax
	mov ax,46	;starting col
	push ax
	mov ah,0x31
	mov al,'-'	;character to print
	push ax
	call horizontalLine

    mov ax,1
	push ax
	mov ax,21  ;starting row
	push ax
	mov ax,45	;starting col
	push ax
	mov ah,0x31
	mov al,'|'	;character to print
	push ax
	call horizontalLine

    mov ax,1
	push ax
	mov ax,21  ;starting row
	push ax
	mov ax,86	;starting col
	push ax
	mov ah,0x31
	mov al,'|'  ;character to print
	push ax
	call horizontalLine


    mov bx,47
    mov cx,38
loopLoading:
    call delay1
    mov ax,1
	push ax
	mov ax,21  ;starting row
	push ax
	mov ax,bx	;starting col
	push ax
	mov ah,0x10
	mov al,0x20  ;character to print
	push ax
	call horizontalLine
    inc bx
    loop loopLoading

	pop cx
	pop bx
	pop ax
	ret

;-------------------------
restoreScreen:
	push bp
	mov bp,sp
	push ax
	push cx
	push es
	push ds
	push si
	push di
	
	;source
	mov ax,screenBuffer
	mov ds,ax
	mov si,0
	
	;destination
	mov ax,0xb800
	mov es,ax
	mov al,132
	mul byte[bp+4]
	shl ax,1
	mov di,ax
	
	mov cx,[bp+6]
	
	cld
	rep movsw
	
	pop di
	pop si
	pop ds
	pop es
	pop cx
	pop ax
	pop bp
	ret 4

;-------------------------
mainScreen:
	push ax
	push bx
	push cx
	call cls
	

	mov ax,0
	push ax
	call sky
	mov ax,14
	push ax
	call sky
	mov ax,28
	push ax
	call sky

	mov ax,2112
	push ax
	mov ax,28
	push ax
	call restoreScreen

	mov bx,12
	mov cx,3
loopMainScreenBoard:
	; screen board
	mov ax,	30		;length of line
	push ax
	mov ax,bx		;row
	push ax
	mov ax,54	;starting col
	push ax
	mov ah,0x10
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	inc bx
	loop loopMainScreenBoard


	mov ax,62	; starting column
	push ax
	mov ax,13	; starting row
	push ax	
	mov ax,0x13	; attribute
	push ax
	mov ax,gameTitle
	push ax
	call printstr

	mov ax,58	; starting column
	push ax
	mov ax,18	; starting row
	push ax	
	mov ax,0x31	; attribute
	push ax
	mov ax,member1
	push ax
	call printstr

	mov ax,58	; starting column
	push ax
	mov ax,19	; starting row
	push ax	
	mov ax,0x31	; attribute
	push ax
	mov ax,member2
	push ax
	call printstr
	

	mov ax,54	; starting column
	push ax
	mov ax,24	; starting row
	push ax	
	mov ax,0x13	; attribute
	push ax
	mov ax,pressEnterText
	push ax
	call printstr

loopMainScreenPressEnter:

	mov ah, 1 		; service 1 – read character
	int 0x21 		; dos services
	cmp al, 13 		; is enter pressed
	je startGame

	jmp loopMainScreenPressEnter


startGame:
	mov word[gameStarted],1		; yes, leave input
	call loadingScreen
	call playGame

	cmp word[gameStarted],0
	jne mainScreenExit
	jmp far[terminate]
mainScreenExit:

	pop cx
	pop bx
	pop ax
	ret
;-------------------------
printScreen:
;Resolution is 132 cols and 43 rows

	push ax

	mov ah,0x00
	mov al, 0x54
	int 0x10
	call cls
	cmp word[gameStarted],0
	jne printGame
	call mainScreen
	jmp printScreenExit
printGame:
	call divideScreen
	call upperPortion
	call middlePortion
	call lowerPortion
printScreenExit:
	pop ax
	ret

;------------------------
timer:
	push ax

	cmp word[cs: gamePaused],1
	je timerExit

	inc word[cs:timerCount]

	cmp word[cs:isRabbitOnBlueTile],0
	je resetBlueTimeCount
	inc word[cs:blueTileCount]
	cmp word[cs:blueTileCount],80	;checking if the time on blue tile has completed
	jl timerExit
	call gameover
	;jmp far[cs:terminate]
	jmp timerExit
resetBlueTimeCount:
	mov word[cs:blueTileCount],0

timerExit:

	mov al,0x20
	out 0x20,al

	pop ax
	iret
;-------------------------
spawnTile:
	push ax
	push bx
	push dx
	push si

	
	mov si,60
	mov ax,[cs:timerCount]
	mov bx,3
	div bx
	cmp dx,1
	je addInTile
	cmp dx,2
	je subInTile
	jmp spawnTiles
addInTile:
	add si,4
	jmp spawnTiles
subInTile:
	sub si,4
	jmp spawnTiles
spawnTiles:
	cmp dx,1
	jne spawnBlueTile
spawnBrownTile:
	mov ax,10	;length of line
	push ax
	mov ax,30		; row
	push ax
	mov ax,si	;starting col
	push ax
	mov ah,0x60
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	jmp spawnTileExit
spawnBlueTile:
	cmp word[movingTileRow],43
	jge spawnBrownTile
	mov ax,10	;length of line
	push ax
	mov ax,30		; row
	push ax
	mov ax,si	;starting col
	push ax
	mov ah,0x10
	mov al,0x20	;character to print
	push ax
	call horizontalLine
spawnTileExit:
	pop si
	pop dx
	pop bx
	pop ax 
	ret

;-------------------------
pauseGame:
	push ax

	mov word[cs:gamePaused],1

	mov ax,1980
	push ax
	mov ax,28
	push ax
	call storeScreen

	mov ax,28
	push ax
	call sky

	mov ax,54	; starting column
	push ax
	mov ax,34	; starting row
	push ax	
	mov ax,0x31	; attribute
	push ax
	mov ax,pauseGameText1
	push ax
	call printstr

	mov ax,54	; starting column
	push ax
	mov ax,35	; starting row
	push ax	
	mov ax,0x31	; attribute
	push ax
	mov ax,pauseGameText2
	push ax
	call printstr

	pop ax
	ret



;-------------------------
kbisr:
    push ax
	push bx
	push cx
	push es
	push di

	cmp word[cs:gameStarted],0
	je nomatch
    in al,0x60
	cmp word[cs:gamePaused],1
	je pausedState
	cmp al,0x01
	je escapePressed
    cmp al,0x39
	je spacePressed


nomatch:
	pop di
	pop es
	pop cx
	pop bx
    pop ax
    jmp far[cs:oldisr]

escapePressed:

	call pauseGame

pausedState:

	cmp al,0x31
	jne pausedStateNextCheck
	mov ax,1980
	push ax
	mov ax,28
	push ax
	call restoreScreen
	mov word[gamePaused],0
	jmp kbisrexit
pausedStateNextCheck:
	cmp al,0x15
	jne nomatch
	mov word[cs:gameStarted],0
	mov word[cs:gamePaused],0
	call gameoverScreen
	;jmp far[cs:terminate]
	jmp kbisrexit
spacePressed:
	call handleScore
	mov word[cs:blueTileCount],0
	mov cx,5
movePlatformDownLoop:
	call delay2
	call movePlatformDown
	loop movePlatformDownLoop


	;drawing rabbit again at new position
	mov ax,39	; row
	push ax
	mov ax,[cs:rabbitCol]	;starting col
	push ax
	call rabbit
	;removing previous rabbit
	mov ax,132	;length of line
	push ax
	mov ax,42		;14th row
	push ax
	mov ax,0	;starting col
	push ax
	mov ah,0x30
	mov al,0x20	;character to print
	push ax
	call horizontalLine
	

	add word[cs:movingTileRow],5
	cmp word[cs:movingTileRow],40	; where the rabbit is present
	jne rabbitOnMovingTileFalse
	mov word[cs:heightOfTile],3
	jmp checkForCollision
rabbitOnMovingTileFalse:
	mov word[cs:heightOfTile],0
checkForCollision:
	;updating the moving tile location so that
	mov ax,0xb800
	mov es,ax

	mov ax,132
	mov bx,39
	mul bx
	add ax, [cs:rabbitCol]
	shl ax,1	;current left bottom position of rabbit
	mov di,ax
	mov ax,132
	shl ax,1
	add di,ax	; to reach the tile

brownTileCheck:
	mov ah,0x60	; checking if the pixel is brown tile
	mov al,0x20
	cmp [es:di],ax
	je setBrownTileCheck
	add di,8
	cmp [es:di],ax
	je setBrownTileCheck
	jmp blueTileCheck
setBrownTileCheck:
	mov word[cs:isRabbitOnBlueTile],0
	jmp collisionTrue 

blueTileCheck:
	mov ah,0x10	; checking if the pixel is blue tile
	mov al,0x20
	cmp [es:di],ax
	je setBlueTileCheck
	sub di,8
	cmp [es:di],ax
	je setBlueTileCheck
	jmp terminateGame
setBlueTileCheck:
	mov word[cs:isRabbitOnBlueTile],1
	jmp collisionTrue
terminateGame:
	call gameover
	;jmp far[cs:terminate]
	jmp kbisrexit
collisionTrue:
	; spwaning new tile
	call spawnTile

	;checking the position of movingtile
	mov ax,43
	cmp word[cs:movingTileRow],ax
	jl updateMovingTileRowVarExit
	mov word[cs:movingTileRow],30
	call spawnCarrot

updateMovingTileRowVarExit:

kbisrexit:
    mov al,0x20
    out 0x20,al
	pop di
	pop es
	pop cx
	pop bx
    pop ax
    iret

;------------------------
spawnCarrot:
	push ax
	push bx
	push dx
	push es
	push di

	mov ax,0xb800
	mov es,ax
	
	mov ax,132
	mov bx,32	;row of the carrot
	mul bx
	add ax,64	; column of the carrot
	push ax
	mov ax,[cs:timerCount]
	mov bx,3
	div bx
	pop ax
	sub ax,4
	cmp dx,0
	je moveCarrotCenter
	cmp dx,1
	je moveCarrotRight
	jmp skipMoveCarrot
moveCarrotCenter:
	add ax,2
moveCarrotRight:
	add ax,4
skipMoveCarrot:

	xor dx,dx
	shl ax,1
	mov di,ax 
	add di,264
	mov ah,0x34 
	mov al,"("
	mov word [es:di],ax

	add di,2
	mov al,")"
	mov word [es:di],ax

	sub di,2
	sub di,264
	mov ah,0x32
	mov al,","
	mov word [es:di],ax
	add di,2
	mov al,","
	mov word [es:di],ax
	
	

	pop di
	pop es
	pop dx
	pop bx
	pop ax

	ret
;------------------------
removeCarrot:
	push ax

	mov ax,7	;length of line
	push ax
	mov ax,32		; row
	push ax
	mov ax,[cs:rabbitCol]	;starting col
	dec ax
	push ax
	mov ah,0x30
	mov al,0x20	;character to print
	push ax
	call horizontalLine

	mov ax,7	;length of line
	push ax
	mov ax,33		; row
	push ax
	mov ax,[cs:rabbitCol]	;starting col
	dec ax
	push ax
	mov ah,0x30
	mov al,0x20	;character to print
	push ax
	call horizontalLine

	pop ax
	ret
;-------------------------
handleScore:
	push ax
	push bx
	push es
	push di

	mov ax,0xb800
	mov es,ax
	
	mov ax,132
	mov bx,33
	mul bx
	add ax,[cs:rabbitCol]
	dec ax	
	shl ax,1

	mov di,ax 
	mov ah,0x34 
	mov al,"("
	mov cx,6
searchCarrot:
	cmp [es:di],ax
	je incrementScore
	add di,2
	loop searchCarrot
	jmp skipIncrementScore
incrementScore:
	inc word[scoreNumber]
	call printScore
	call removeCarrot
	
skipIncrementScore:

	pop di
	pop es
	pop bx
	pop ax

	ret


;-------------------------
moveLeft:
	; starting row bp+6
	; ending row bp+4
	
	push bp
	mov bp,sp
	push ax
	push bx
	push dx
	push cx
	push es
	push di
	push si

	mov ax,132
	mul word[bp+6]	;starting row
	shl ax,1
	mov di,ax 
	
	mov ax,[bp+4]
	sub ax,[bp+6]
	inc ax
	mov bx,132
	mul bx
	sub ax,1
	mov cx,ax
	
	mov bx,1
	mov dx,132	; dx contains the number of columns
	mov ax,0xb800
	mov es,ax
moveLeftLoop:
	cmp bx,dx
	jne moveLeftSkip
	mov ax,di
	push bx
	mov bx,dx
	dec bx 		; 0 based indexing
	shl bx,1
	sub ax,bx
	mov si,ax
	pop bx
	mov bx,0
	jmp continuemoveLeftLoop
moveLeftSkip:
	mov si,di
	add si,2
continuemoveLeftLoop:
	mov ax,[es:si]
	mov [es:di],ax
	add di,2
	inc bx
	loop moveLeftLoop
	
	pop si
	pop di
	pop es
	pop cx
	pop dx
	pop bx
	pop ax
	pop bp
	ret 4
;-------------------------
moveRight:
	; starting row bp+6
	; ending row bp+4
	push bp
	mov bp,sp
	push ax
	push bx
	push dx
	push cx
	push es
	push di
	push si
	mov ax,132	; 132
	mul word[bp+4]			; 132 * ending row
	mov bx,132
	dec bx
	add ax,bx	; add 132 to reach end of that row
	shl ax,1				; 2 byte system
	mov di,ax 				; moving into di and si
	
	mov ax,[bp+4]
	sub ax,[bp+6]
	inc ax
	mov bx,132
	mul bx
	sub cx,1
	mov cx,ax
	
	mov bx,1
	mov dx,132	; dx contains the number of columns
	mov ax,0xb800
	mov es,ax
moveRightLoop:
	cmp bx,dx
	jne moveRightSkip
	mov ax,di	;moving current di into ax
	push bx
	mov bx,dx	; moving total number of columns into dx
	dec bx 		; 0 based indexing
	shl bx,1	; 2 byte system
	add ax,bx	; to reach the end column of current row
	mov si,ax
	pop bx
	mov bx,0
	jmp continuemoveRightLoop
moveRightSkip:
	mov si,di
	sub si,2
continuemoveRightLoop:
	mov ax,[es:si]
	mov [es:di],ax
	sub di,2
	inc bx
	loop moveRightLoop
	
	pop si
	pop di
	pop es
	pop cx
	pop dx
	pop bx
	pop ax
	pop bp
	ret 4

;
;-------------------------
movePlatformDown:
	push ax
	push bx
	push cx
	push es
	push di
	push si

	;to reach the end of last row
	mov ax,132	;132
	mov bx,42	;last row 
	mul bx		;132 * row
	mov bx,132
	dec bx
	add ax,bx	;adding total cols to reach end of last row
	shl ax,1

	mov di,ax
	mov ax,132	;132*2 = 264
	shl ax,1	
	mov si,di	
	sub si,ax			;to reach the last column of upper row

	mov ax,42	;ending row
	sub ax,29	;starting row of third portion

	mov bx,132
	mul bx		;ans * 132

	mov cx,ax
	mov ax,0xb800
	mov es,ax
movePlatformDownLoop1:
	mov ax,[es:si]
	mov [es:di],ax
	sub di,2
	sub si,2
	loop movePlatformDownLoop1

	
	pop si
	pop di
	pop es
	pop cx
	pop bx
	pop ax
	ret
;-------------------------
moveLower:
	push ax
moveLowerSkip:

	cmp word[tileMovementVar],35
	jl moveTileRight
moveTileLeft:
	mov ax,[movingTileRow]
	sub ax,[heightOfTile]
	push ax
	mov ax,[movingTileRow]
	add ax,1
	push ax
	call moveLeft

	cmp word[heightOfTile],0	;checking if the rabbit is present on the tile or not
	je skipRabbitColumnDec
	dec word[rabbitCol]			;<- decrement
skipRabbitColumnDec:

	inc word[tileMovementVar]
	cmp word[tileMovementVar],70
	jl moveLowerEnd
	mov word[tileMovementVar],0
	jmp moveLowerEnd
moveTileRight:
	mov ax,[movingTileRow]
	sub ax,[heightOfTile]
	push ax
	mov ax,[movingTileRow]
	add ax,1
	push ax
	call moveRight

	cmp word[heightOfTile],0	;checking if the rabbit is present on the tile or not
	je skipRabbitColumnInc
	inc word[rabbitCol]			;-> increment
skipRabbitColumnInc:

	inc word[tileMovementVar]
moveLowerEnd:
	pop ax
	ret

;-------------------------              moveLowerEnd                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            moveLowerEnd                                                   moveLowerEnd                                                                                                                           
moveMiddle:
	push ax
	mov ax,17	; starting row
	push ax
	mov ax,20	;ending row
	push ax
	call moveLeft

	mov ax,23
	push ax
	mov ax,27
	push ax
	call moveLeft
	pop ax
	ret
;-------------------------
moveUpper:
	push ax

	mov ax,0	; starting row
	push ax
	mov ax,16	;ending row
	push ax
	call moveRight
	pop ax
	ret
;-------------------------
storeScreen:
	push bp
	mov bp,sp
	push ax
	push cx
	push es
	push ds
	push di
	push si
	
	
	;destination
	mov ax,screenBuffer
	mov es,ax
	mov di,0
	
	;source
	mov ax,0xb800
	mov ds,ax
	mov al,132
	mul byte [bp+4]
	shl ax,1
	mov si,ax
	
	mov cx,[bp+6]
	cld 
	rep movsw

	
	pop si
	pop di
	pop ds
	pop es
	pop cx
	pop ax
	pop bp
	ret 4
;-------------------------
movement:

movementLoop:
	cmp word[gameStarted],1
	jne movementExit
	call delay1
	call moveUpper
	call moveMiddle
	cmp word[gamePaused],1
	je movementLoop
	call moveLower
	jmp movementLoop
movementExit:

	ret
	
;--------------------------

playGame:

	call printScreen
	call movement

	ret
;-------------------------
start:
	xor ax,ax
    mov es,ax
    mov ax,[es:9*4]
    mov [oldisr],ax
    mov ax,[es:9*4+2]
    mov [oldisr+2],ax
    cli
    mov word[es:9*4],kbisr
    mov [es:9*4+2],cs 

	mov word[es:8*4],timer
	mov [es:8*4+2],cs
    sti
    mov dx,start
    add dx,15
    mov cl,4
    shr dx,cl

	mov word[gameStarted],1
	call printScreen
	mov ax,2112
	push ax
	mov ax,0
	push ax
	call storeScreen
	mov word[gameStarted],0

	call printScreen

	
terminate:


mov ax,0x4c00
int 0x21
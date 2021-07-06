
//;	@com.wudsn.ide.asm.outputfileextension=.atr

//; include files, useful macros:
//	icl "../base/sys_equates.m65"
//	icl "../base/sys_macros.m65"

start_data =$2000
start_cartridge =$a000
cart_length =$2000
buffer = $6000
cart_off =$D5FF

write_enable = $40
write_enable2 = $04

dir_lines = 19
num_files = dir_lines*2

pt_1 = $cb
pt_2 = $cd
pt_3 = $cf


	opt h-
	
//Let's do the ATR Header

Total_Size		= (((end_atr - start_data)+127)/128)*128
Total_Size_high	= (Total_Size/$100000)
Total_Size_low	= (Total_Size-(Total_Size_high*$100000))/$10

Sectors_boot	= (end_program-start_data+127)/128

init_sector		= Sectors_boot + 1
;init_rom_sector	= (init_sector + (starting_bank*$40))
init_rom_sector	= init_sector

//	.by $96,$02				//Checksum of "NICKATARI"
//	.word Total_Size_low	//Total bytes
//	.word $80				//Sector Size
//	.word Total_Size_high	//Total bytes
//	.by $00					//No Write Protect nor Bad sectors.
//	.word $00				//No bad sectors
//	.by $00,$00,$00,$00,$00


//	org start_data
	
//	.by $00
//	.by Sectors_boot
//	.word start_data
//	.word start
//init_boot
//	clc
//	rts	



//Determinamos macros

//Partimos con seleccionar el banco
//Formato: banco x,y
//x = Cartridge bank to activate
//y= 0 if its for reading, Y=1 read/write
.macro banco
.if :0 <> 2 
	.error "banco sin parámetros"
.else
	.if :1 < 256
		lda #:1
		.if :2 = 1
			ora #write_enable
		.endif
		tax
		sta $d500,x
	.else
		lda :1
		.if :2 = 1
			;ora #write_enable
			ora value_write
		.endif
		tax
		sta $d500,x
	.endif
.endif 
.endm

//Realiza un print en la pantalla hasta encontrar un RETURN ($9b).
//Uso: print mem_addr
.macro print
.if :0 <> 1
	.error "print sin parámetros"
.else
	ldx #<:1
	ldy #>:1
	jsr pr
.endif
.endm
	

//print2: imprime una cantidad fija de caracteres según un largo definido.
//Uso: print2 mem_addr, num_caracteres
.macro print2
.if :0 <> 2
	.error "print2 sin parámetros"
.else
	ldx #<:1
	ldy #>:1
	lda #:2
	jsr pr2
.endif
.endm



//Cierra el canal solicitado.
//Uso: close channel
.macro close
.if :0 <> 1
	.error "close sin parámetros"
.else
;	lda #:1
;	:+4 asl
;	tax
	ldx #(:1*16)
	lda #$0C	//Close
	sta ICCMD,x
	jsr CIOV
.endif
.endm

//Open: open CIO channel, simmilar to BASIC.
//Uso: open channel,operation,aux,loc_handler

.macro open
.if :0 <> 4
	.error "open sin parámetros"
.else
;	lda #:1
;	:+4 asl
;	tax
	ldx #(:1*16)
	mva #$03 ICCMD,x	//Open
	mwa #:4	ICBAL,x		//"E:","C:","D:"
	mva #:2	ICAX1,x		//Read and write
	mva #:3 ICAX2,x
	jsr CIOV
.endif
.endm

//CopyM: Copy número de páginas de una dirección a otra.
.macro CopyM
//Restricción: sólo utiliza los bytes mayores. Para un movimiento más fino, utilizar CopyMemory

.if :0 <> 3
	.error "CopyM sin parámetros correctos"
.else
	ldx #>:1
	ldy #>:2
	lda #>:3
	jsr copy
.endif


.endm

//WriteMemory: Escribe en disco el contenido de memoria
//Parámetros:
//  1: Canal a escribir
//  2: Zona de memoria inicial a escribir
//  3: Cantidad de bytes
.macro WriteMemory
.if :0 <> 3
	.error "WriteMemory sin parámetros correctos"
.else
;	lda #:1
;	:+4 asl
;	tax
	ldx #(:1*16)
	mwa #:2 ICBAL,x
	mwa #:3 ICBLL,x
	mva #11 ICCMD,X
	jsr ciov
.endif
.endm

//ReadDisk: lee del disco el contenido y guarda en memoria
//Parámetros:
//  1: Canal del cual leer
//  2: Zona de memoria al cual grabar
//  3: Cantidad de bytes
.macro ReadDisk
.if :0 <> 3
	.error "ReadDisk sin parámetros correctos"
.else
;	lda #:1
;	:+4 asl
;	tax
	ldx #(:1*16)
	mwa #:2 ICBAL,x
	mwa #:3 ICBLL,x
	mva #7 ICCMD,X
	jsr ciov
.endif
.endm


//WriteCart: Escribe en cartridge el contenido de memoria
//Parámetros:
//  1: Canal a escribir
//  2: Zona de memoria inicial a escribir
//  3: Cantidad de bytes
.macro WriteCart
.if :0 <> 3
	.error "WriteCart sin parámetros correctos"
.else
	ReadDisk :1,:2,:3
	CopyM :2,start_cartridge,:3
.endif
.endm

//input: realiza un input por CIO
//Uso: input canal,dirección a tomar
.macro input
.if :0 <> 2
	.error "Input sin parámetros correctos"
.else
;	lda #:1
;	:+4 asl
;	tax
	ldx #(:1*16)
	mwa #:2 ICBAL,X		;Dirección entregada
	mwa #20 ICBLL,x		;Máximo 20 bytes (Pueden ser más)
	mva #5 ICCMD,x		;Comando INPUT
	jsr ciov		;Ejecute"
.endif

.endm

//get: realiza un input por CIO
//Uso: input canal,dirección a tomar
.macro get
.if :0 <> 2
	.error "Get sin parámetros correctos"
.else
;	lda #:1
;	:+4 asl
;	tax
	ldx #(:1*16)
	mwa #:2 ICBAL,X		;Dirección entregada
	mwa #1 ICBLL,x		;Cantidad de bytes = 1
	mva #7 ICCMD,x		;Realizar una lectura
	jsr ciov		;Ejecute!
.endif

.endm


boot_sector
	.by $00
	.by $01		//1 sector to read
	.word $600
	.word $606
	sec
	rts
	:120 .by $00

LOC_E
	.by "E:",$9B	;Handler editor E:
LOC_K
	.by "K:",$9B	;Handler keyboard
LOC_DISK
	.by "D1:AAAAAAAA.DMP",$9B	;file name (not used now)
LOC_DIR
	.by "D1:*.*",$9B		;search directory (not used)
LOC_INPUT
	.by "                    "
dl
	.by $70,$70,$70
	:7 .by $70
	.by $30
	.by $42				;Primera línea modo ANTIC 2
	.word screen
	:4 .by $02 			;4 líneas más
	.by $70
	.by $02,$02,$02
	.by $41				;Vuelva
	.word dl			;al display list!
	

screen
	.sb +32,"QRRRRRRRRRRRRRRRRRRRRRRRRRRRRWRRRRRRRRRE"
	.sb     "|AR Dungeon Char Transfer 1.1|Willysoft|"
	.sb +32,"ARRRRRRRRRRRWRRRRRRRRRRRRRRRRXRRRRRRRRRD"
	.sb     "|"
	.sb +128,"START"
	.sb      ":"
screen_start
	.sb "BEGIN"
screen_start_end

	.sb "|"
	.sb +128,"SELECT"
	.sb ":"
screen_operation
	.sb "CARTRIDGE TO DISK  "
screen_operation_end
len_sc_operation= screen_operation_end - screen_operation
	.sb "|"
	.sb +32,"ZRRRRRRRRRRRXRRRRRRRRRRRRRRRRRRRRRRRRRRC"

sc_screen_format	
	.sb "                                        "
	.sb "                                        "
	.sb "                                        "

sc_format
	.sb +32,"QRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRE"
	.sb     "|   PRESS "
	.sb +128,"RETURN"
	.sb "  TO FORMAT DISKETTE   |"
	.sb +32,"ZRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRC"
sc_format_end
len_sc_format=sc_format_end-sc_format

sc_cart_error
	.sb +32,"QRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRE"
	.sb     "|   "
	.sb +128,"ERROR:"
	.sb " NO CHARACTER ON CARTRIDGE   |"
	.sb +32,"ZRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRC"
sc_cart_error_end
len_sc_cart_error = sc_cart_error_end - sc_cart_error


sc_disk_error
	.sb +32,"QRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRE"
	.sb     "|   "
	.sb +128,"ERROR:"
	.sb "  NO CHARACTER ON DISKETTE   |"
	.sb +32,"ZRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRC"
sc_disk_error_end
len_sc_disk_error = sc_disk_error_end - sc_disk_error


sc_cart				;00
	.sb "CARTRIDGE TO DISK  "
sc_disk
	.sb "DISK TO CARTRIDGE  "

text_check
	.by "AR CHAR!"	//Header to check on disk or cartridge
text_check_end
len_text_check = text_check_end - text_check

BEEP
	.by $FD		//Beep sound

flag_busy
	.by $00

flag_operation
	.by $00
flag_start
	.by $00

consol_previous
	.by $08

key
	.by $00
	
//Format cartridge routine for AR Dungeon.
.proc format_cartridge
	lda #$0a
	jsr erasebk	//Erase bank $0a (Header info)
	lda #$0b
	jsr erasebk	//Erase bank $0b (save state 1)
	lda #$0c
	jsr erasebk	//Erase bank $0c (save state 2)
	lda #$0d
	jsr erasebk	//Erase bank $0d (save state 3)
	lda #$0e
	jsr erasebk	//Erase bank $0e (save state 4)
	lda #$ff
	sta cart_off
	ldy #$01	// All done without errors
	rts		//Formatting successful
.endp

.proc setsec
	and #$0F	//Only $00-$0F allowed
	clc		//Just to not set bit 7 to 1 accidentally
	rol		//*2
	rol		//*4
	rol		//*8
	tax
	sta $d500,x	//Change bank!
	rts
.endp

.proc erasebk
	pha
	stx temp_x
	
	jsr cmd_unlock		//First two cycles!
	lda #$80
	jsr wr5555		//Third cycle!
	jsr cmd_unlock		//Fourth and fifth cycles!
	pla
	pha
	jsr setsec
	lda #$30		//Sixth and final cycle!
	sta start_cartridge	//Erase!
	jsr poll_write

	jsr CheckID
	cmp #$bf			//Is 39F?
	bne erasebk_exit

	jsr cmd_unlock		//First two cycles!
	lda #$80
	jsr wr5555		//Third cycle!
	jsr cmd_unlock		//Fourth and fifth cycles!
	pla
	pha
	jsr setsec
	lda #$30		//Sixth and final cycle!
	sta start_cartridge+$1000	//Erase!
	jsr poll_write2
		
erasebk_exit
	ldx temp_x
	pla
	rts
	
//Poll_write: wait until the erase is finished.
poll_write
	lda #$00
	sta pollsame
@poll_again
	lda start_cartridge
	cmp start_cartridge
	bne poll_write
	cmp start_cartridge
	bne poll_write
	cmp start_cartridge
	bne poll_write
	inc pollsame
	bne @poll_again
	lda #$ff
	sta cart_apaga
	rts
	
poll_write2
	lda #$00
	sta pollsame
@poll_again2
	lda start_cartridge+$1000
	cmp start_cartridge+$1000
	bne poll_write
	cmp start_cartridge+$1000
	bne poll_write
	cmp start_cartridge+$1000
	bne poll_write
	inc pollsame
	bne @poll_again2
	lda #$ff
	sta cart_apaga
	rts
pollsame
	.by $00
temp_x	.by $00
.endp
.proc CheckID
	stx CheckID_save_x+1
	jsr cmd_unlock
	lda #$90
	jsr Wr5555
	sta $d540
	lda $a000
	ora $a001
	pha
	jsr cmd_unlock
	lda #$f0
	jsr Wr5555
	ldx #$00
CheckID_loop
	inx
	bne CheckID_loop

CheckID_save_x
	ldx #$00
	pla
	rts
.endp
	
.proc wr5555
	sta $d542	//Setting third bank from second chip.
	sta $b555	//Store on $5555!
	rts
.endp

.proc cmd_unlock
	lda #$AA	//Store $aa on $5555
	jsr wr5555	//Do it!
	lda #$55	//Store $55 on $2aaa
.endp

.proc wr2AAA
	sta $d541	//Setting second bank from second chip.
	sta $aaaa	//Store on $2aaa
	rts

.endp

.proc enable_write
	stx temp_x
	pha
	jsr cmd_unlock	//First and second cycle
	lda #$a0
	jsr wr5555	//Third cycle
enable_write_cont
	pla
	tax
	sta $d500,x
	ldx temp_x
	rts
temp_x
	.by $00
.endp

.proc WriteM
	WriteMemory 1,start_cartridge,cart_length
	rts
.endp

.proc get_key
	get 1,key
	rts
.endp

.proc wait_return
start
	lda CH
	cmp #$ff	//Key pressed?
	bne start	//Nope, start over!
	jsr get_key //Get key
	lda #$ff	//Erase last key pressed
	sta CH		//Done!
	lda key		//Read key
	cmp #$9b	//Is it Return?
	bne start	//No, let's do it again.
	rts			//Yes, return
.endp

.proc format_disk
	mva #$31 DDEVIC
	mva #$01 DUNIT
	mva #$21 DCOMND
	mva #$80 daux1
	mva #$00 daux2
	jsr siov
	rts
	
.endp

.proc write_disk

//Character 1
	banco $58,0
	ldx #$03
	ldy #$00
	jsr write_char
//Character 2
	banco $60,0
	ldx #$bb
	ldy #$00
	jsr write_char
//Character 3
	banco $68,0
	ldx #$73
	ldy #$01
	jsr write_char
//Character 4
	banco $70,0
	ldx #$2b
	ldy #$02
	jsr write_char
	rts
.endp

.proc write_char
	stx daux1
	sty daux2
	mwa #$a000 dbuflo
	mva #$50 dcomnd
	mva #$26 sec_count	//$26 sectors to write.
loop
	jsr dskinv
	inc daux1
	bne noincdaux2
	inc daux2
noincdaux2
	clc
	lda dbuflo
	adc #$80
	sta dbuflo
	bcc noincdbufhi
	inc dbufhi
noincdbufhi
	dec sec_count
	bne loop
	rts

sec_count
	.by $00
.endp


.proc write_cartridge
	ldx #$01			//First sector LSB
	ldy #$00			//First sector MSB
	lda #$02			//2 sectors to read
	jsr read_char		//Do it!
	lda #$50			//Bank $50 to write (Sector $A
	ldx #$01			//$100 bytes to write
	jsr write_char_cart	//Do it!


//First character
	ldx #$03			//Sector 3 LSB
	ldy #$00			//Sector 3 MSB
	lda #$26			//$26 sectors to read
	jsr read_char		//Do it!
	lda #$58			//Bank $58 (Sector $B)
	ldx #$13			//$1300 bytes to write
	jsr write_char_cart	//Do it!

//Second character
	ldx #$bb
	ldy #$00
	lda #$26
	jsr read_char
	lda #$60
	ldx #$13
	jsr write_char_cart

//Third character
	ldx #$73
	ldy #$01
	lda #$26
	jsr read_char
	lda #$68
	ldx #$13
	jsr write_char_cart

//Fourth character
	ldx #$2b
	ldy #$02
	lda #$26
	jsr read_char
	lda #$70
	ldx #$13
	jsr write_char_cart
	
	rts
.endp

.proc read_char
	stx daux1
	sty daux2
	sta sec_count
	mva #$52 dcomnd
	mwa #buffer dbuflo
loop
	jsr dskinv
	inc daux1
	bne noincdaux2
	inc daux2
noincdaux2
	lda dbuflo
	clc
	adc #$80
	sta dbuflo
	bcc noincdbufhi
	inc dbufhi
noincdbufhi
	dec sec_count
	bne loop
	rts
sec_count
	.by $00
.endp

.proc write_char_cart
	stx pt_3+1
	sta bank
	mva #$00 pt_3
	mwa #buffer pt_1
	mwa #$a000 pt_2
loop
	lda bank
	jsr enable_write
	ldy #$00
	lda (pt_1),y
	sta (pt_2),y
	inc pt_1
	bne noinc_pt1_1
	inc pt_1+1
noinc_pt1_1
	inc pt_2
	bne noinc_pt2_1
	inc pt_2+1
noinc_pt2_1
	lda pt_3
	bne nodec_pt3_1
	dec pt_3+1
nodec_pt3_1
	dec pt_3
	lda pt_3
	ora pt_3+1
	bne loop
	rts
bank
	.by $00
.endp

.proc cart_check
	banco $50,0
	ldx #len_text_check-1
loop
	lda $a080,x
	cmp text_check,x
	bne no_check
	dex
	bpl loop
	clc
	rts
no_check
	sec
	rts
.endp

.proc disk_check
	mva #$31 ddevic
	mva #$01 dunit
	mva #$52 dcomnd
	mwa #$0002 daux1
	mwa #buffer dbuflo
	jsr dskinv
	ldx #len_text_check-1
loop
	lda buffer,x
	cmp text_check,x
	bne no_check
	dex
	bpl loop
	clc
	rts
no_check
	sec
	rts
.endp
.proc error_cart
	ldx #len_sc_format-1
loop
	lda sc_cart_error,x
	sta sc_screen_format,x
	dex
	bpl loop
	writememory 0,beep,1
	rts
.endp

.proc error_disk
	ldx #len_sc_format-1
loop
	lda sc_disk_error,x
	sta sc_screen_format,x
	dex
	bpl loop
	writememory 0,beep,1
	rts
.endp

.proc vbi
	lda trig3
	sta gintlk		//No cartridge lockup!
	lda atract
	bmi vbi_end
	lda #$00
	sta atract
vbi_end
	jmp sysvbv
.endp

.proc vbd
	lda flag_busy
	bne vbd_end
	lda consol
	cmp consol_previous
	jeq vbd_end
	sta consol_previous
	lsr		; START?
	bcs vbd_2	; No!
	pha

	lda #$01
	sta flag_start

	pla
vbd_2
	lsr 		; SELECT?
	bcs vbd_3	; No!
	pha
	ldx #37
	ldy #18
	lda flag_operation
	eor #$01
	sta flag_operation
	bne vbd_2_write
	ldx #18
vbd_2_write
	lda sc_cart,x
	sta screen_operation,y
	dex
	dey
	bpl vbd_2_write
	pla
vbd_3
	lsr		;OPTION?
	bcs vbd_end	;NO!
;	lda #$01
;	sta flag_dir
vbd_end
	jmp xitvbv
.endp

.proc screen_erase
	lda #$00
	ldx #len_sc_format-1
loop
	sta sc_screen_format,x
	dex
	bpl loop
	rts
.endp
	
start
	jsr screen_erase
start2
	lda portb
	ora #$fe	//No BASIC, maintain SIO patch if there's any.
	sta portb
	mva #$01 basicf
	close 0
	close 1
	mva #$a0 RAMTOP
	open 0,12,0,LOC_E
	open 1,4,0,LOC_K
	mwa #dl SDLSTL
	mva #$00 flag_busy	//just a flag
	mva #$00 color2
	mva #$0f color1
	ldx #>vbi
	ldy #<vbi
	lda #VBI_I
	jsr setvbv			//Set VBI to prevent cartridge lockup
	ldx #>vbd
	ldy #<vbd
	lda #VBI_D
	jsr setvbv			//VBD to look on consol keys.

loop_start
	lda flag_start
	beq loop_start

	jsr screen_erase
	sta flag_busy
	dec flag_start
	lda flag_operation		//DISK to CART or CART to DISK?
	jne no_to_disk
	jsr cart_check
	bcc no_cart_error
	jsr error_cart
	jmp start2

no_cart_error
	ldx #len_sc_format-1
start_loop_sc_format
	lda sc_format,x
	sta sc_screen_format,x
	dex
	bpl start_loop_sc_format
	
	
	writememory 0,beep,1	//Make a beep sound! That's the legal way
	jsr wait_return
	jsr format_disk
	mwa #$0001 daux1
	mwa #boot_sector dbuflo
	mva #$50 dcomnd
	jsr dskinv
	banco $50,0
	mwa #$a080 dbuflo
	inc daux1
	jsr dskinv
	jsr write_disk
	jmp start


no_to_disk
	jsr disk_check
	bcc no_disk_error
	jsr error_disk
	jmp start2

no_disk_error	
	jsr format_cartridge
	jsr write_cartridge
	jmp start

end_program
end_atr


//	run start

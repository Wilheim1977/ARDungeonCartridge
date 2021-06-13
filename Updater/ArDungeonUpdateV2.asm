
;	@com.wudsn.ide.asm.outputfileextension=.atr


; include files, useful macros:
	icl "../base/sys_equates.m65"
	icl "../base/sys_macros.m65"


start_data =$2000
//start_cartridge =$a000
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

starting_bank 	= $40
final_bank		= $57
total_banks		= final_bank-starting_bank+1
starting_sector	= starting_bank/8
total_sectors	= (total_banks+7)/8

.macro PUT_VERSION
	.sb "V12"
.endm



	opt h-
	
//Let's do the ATR Header

Total_Size		= (((end_atr - start_data)+127)/128)*128
Total_Size_high	= (Total_Size/$100000)
Total_Size_low	= (Total_Size-(Total_Size_high*$100000))/$10

Sectors_boot	= (end_program-start_data+127)/128

init_sector		= Sectors_boot + 1
;init_rom_sector	= (init_sector + (starting_bank*$40))
init_rom_sector	= init_sector

	.by $96,$02				//Checksum of "NICKATARI"
	.word Total_Size_low	//Total bytes
	.word $80				//Sector Size
	.word Total_Size_high	//Total bytes
	.by $00					//No Write Protect nor Bad sectors.
	.word $00				//No bad sectors
	.by $00,$00,$00,$00,$00


	org start_data
	
	.by $00
	.by Sectors_boot
	.word start_data
	.word start
init_boot
	clc
	rts	


// Include Flash drivers	
	icl "FlashCartDriver.asm"


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
//	.by $70
//	.by $02,$02,$02
	.by $41				;Vuelva
	.word dl			;al display list!
	

screen
	.sb +32,"QRRRRRRRRRRRRRRRRRRRRRRRWRRRRRRRRRRRRRRE"
	.sb     "|AR Dungeon Updater "
	PUT_VERSION
	.sb " |2021 Willysoft|"
	.sb +32,"ARRRRRRRRRRRWRRRRRRRRRRRXRRRRRRRRRRRRRRD"
	.sb     "|"
	.sb +128,"START"
	.sb      ":"
screen_start
	.sb "BEGIN"
screen_start_end

screen_status
	.sb "|                          |"
screen_status_end
len_screen_status = screen_status_end - screen_status

	.sb +32,"ZRRRRRRRRRRRXRRRRRRRRRRRRRRRRRRRRRRRRRRC"

screen_erasing
	.sb "|Erasing Cartridge...      |"
screen_erasing_end
len_screen_erasing = screen_erasing_end - screen_erasing

screen_veryfing
	.sb "|Verifying erase...        |"
screen_veryfing_end
len_screen_veryfing = screen_veryfing_end - screen_veryfing

screen_reading
	.sb "|Reading Bank     "
screen_reading_bank
	.sb "00/"
screen_reading_total_banks
	.sb "7F    |"
screen_reading_end
len_screen_reading = screen_reading_end - screen_reading

screen_writing
	.sb "|Writing Bank     "
screen_writing_bank
	.sb "00/"
screen_writing_total_banks
	.sb "7F    |"
screen_writing_end
len_screen_writing = screen_writing_end - screen_writing

screen_verify_write
	.sb "|Verifying Bank   "
screen_verify_write_bank
	.sb "00/"
screen_verify_total_banks
	.sb "7F    |"
screen_verify_write_end

len_screen_verify_write = screen_writing_end - screen_writing

screen_done
	.sb "|DONE!                     |"
screen_done_end
len_screen_done = screen_done_end - screen_done

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
	
banknum
	.by $00
	
count
	.by $00
	
sector
	.word $00
	
tabla_hex
	.sb "0123456789ABCDEF"
	
.proc put_banknum
	lda banknum
	:4 lsr
	tax
	lda tabla_hex,x
	sta screen_reading_bank
	sta screen_writing_bank
	sta screen_verify_write_bank
	lda banknum
	and #$0f
	tax
	lda tabla_hex,x
	sta screen_reading_bank+1
	sta screen_writing_bank+1
	sta screen_verify_write_bank+1
	rts
.endp

.proc put_final_bank
	lda #final_bank
	:4 lsr
	tax
	lda tabla_hex,x
	sta screen_reading_total_banks
	sta screen_writing_total_banks
	sta screen_verify_total_banks
	lda #final_bank
	and #$0f
	tax
	lda tabla_hex,x
	sta screen_reading_total_banks+1
	sta screen_writing_total_banks+1
	sta screen_verify_total_banks+1
	rts
.endp

	
.proc put_erase
	CopyMemory screen_erasing,screen_status,len_screen_erasing
	rts
.endp

.proc put_veryfing
	CopyMemory screen_veryfing,screen_status,len_screen_veryfing
	rts
.endp

.proc put_reading
	CopyMemory screen_reading,screen_status,len_screen_reading
	rts
.endp

.proc put_writing
	CopyMemory screen_writing,screen_status,len_screen_writing
	rts
.endp

.proc put_verify_write
	CopyMemory screen_verify_write,screen_status,len_screen_verify_write
	rts
.endp

.proc put_done
	CopyMemory screen_done,screen_status,len_screen_done
	rts
.endp

.proc WriteM
	WriteMemory 1,start_cartridge,cart_length
	rts
.endp

.proc get_key
	get 1,key
	rts
.endp

.proc Erase_patch
	ldx #starting_sector
	ldy #total_sectors
	jsr Erase_Sectors
	rts
.endp

.proc Verify_Erase
	ldx #starting_sector
	ldy #total_sectors
	jsr VerifyBlankSectors
	rts
.endp

.proc Read_bank
	mva #$31 DDEVIC
	mva #$01 DUNIT
	mwa sector $30A
	mva #$52 dcomnd
	mwa #buffer dbuflo
	mva #$40 count
loop
	jsr dskinv
	bmi loop
	inc daux1
	bne loop2
	inc daux2
loop2
	lda dbuflo
	clc
	adc #$80
	sta dbuflo
	bcc loop3
	inc dbufhi
loop3
	dec count
	bne loop
	mwa daux1 sector
	rts
count
	.by $00
.endp	

.proc Write_bank
	pha
	txa
	pha
	tya
	pha

	mwa #buffer pt_1
	mwa #start_cartridge pt_2
	ldy #$00
	ldx #$20
loop
	lda banknum
	jsr enable_write
	lda (pt_1),y
	sta (pt_2),y
	iny
	bne loop
	inc pt_1+1
	inc pt_2+1
	dex
	bne loop

	pla
	tay
	pla
	tax
	pla
	rts
.endp

.proc Verify_Bank
	pha
	txa
	pha
	tya
	pha
	mwa #start_cartridge pt_1
	mwa #buffer pt_2
	ldy #$00
	lda banknum
	tax
	sta $d500,x
	ldx #$20
loop
	lda (pt_1),y
	cmp (pt_2),y
	bne error
	iny
	bne loop
	inc pt_1+1
	inc pt_2+1
	dex
	bne loop
	clc
	bcc end_verify
error
	sec
end_verify
	pla
	tay
	pla
	tax
	pla
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

.proc Wait_sec
	lda rtclock
	clc
	adc #60
loop
	cmp rtclock
	bne loop
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
vbd_end
	jmp xitvbv
.endp

	
start
//	jsr screen_erase
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
	
//Start main process
	mva #$01 flag_busy	//Put it as busy
	mva #starting_bank banknum
	mwa #init_rom_sector sector
	dec flag_start	
	jsr put_final_bank

// 1st step: Erase cartridge
First_step
	jsr put_erase
	
.if starting_bank =$00
	.if final_bank =$7f
		jsr Erase_Cartridge
	.else
		jsr Erase_patch
	.endif
.else
	jsr Erase_patch
.endif


// 2nd step: Verify erase
	jsr put_veryfing
	jsr Verify_Erase
	bcs First_step
	

	mva #total_banks count
loop
	jsr put_banknum

	jsr put_reading
	jsr Read_bank

error_write	
	jsr put_writing
	jsr Write_bank

	jsr put_verify_write
	jsr Verify_bank
	bcs error_write
	
	inc banknum
	dec count
	bne loop	
	
	
	jsr put_done
	lda #$00
	sta flag_busy
	jmp loop_start
	
end_program	
	
total_program	= end_program-start_data+1
total_temp		= total_program + 127
total sectors	= total_temp/128
total_bytes		= ((Total_Program+127)/128)*128
filler			= total_bytes - total_program +1

.if filler > 0
	.sav filler
.endif

	
start_rom

	ins "../BOOTCartridge/Alternate Reality - The dungeon/Ardungeon.rom",starting_bank*$2000,total_banks*$2000

end_rom

end_atr
	
	
//	run start
		
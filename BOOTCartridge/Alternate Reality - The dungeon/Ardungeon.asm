// ****************************************************
// *                                                  *
// * Generic BOOT Cartridge routine                   *
// *                                                  *
// *     (C) Guillermo Fuenzalida (Willysoft)         *
// *                                                  *
// ****************************************************

//Alternate Reality: The Dungeon Cartridge conversion

//History:

//V8: various bug fixes and optimized D1: access
//V7: Added D1:functionality from physical drive.
//V6: relocating initialization routines, now it boots faster! Changed title credits.
//	Removed SPACE BAR waiting routine when saving character.
//	Removed SPACE BAR waiting routing when resuming character.
//V5: added OPTION key detection. If pressed, then boots the disk. Optimizing booting sequence
//V4: added writing header and character data on flash cartridge. Now it's fully cartridge operational.
//V3: added format character disk.
//V2: bug fixes, now the game loads and work. Need floppy disk to save character.
//V1: initial release. Can load the menu. Game doesn't work.

// First, we will define the extension as .rom

;	@com.wudsn.ide.asm.outputfileextension=.rom

// Now we set no binary headers

	opt h-

; include files, useful macros:
	icl "../../base/sys_equates.m65"
	icl "../../base/sys_macros.m65"
	
// Now a cart type definition

//Cartridge types available
// 1: Atarimax 8 Mbit
// 2: Switchable XEGS 8 Mbit
// 3: Atarimax 1 Mbit
// 4: Atrax 128 Kbytes (1 Mbit)
// 5: Megacart 128 KB
// 6: Megacart 1024 KB

TYPE_CART =1

.if TYPE_CART = 1 
	start_cartridge = $A000
	end_bank = $C000
	cart_banks = 128
	cart_apaga = $D5FF
	FLAG_16KB = $00
.elseif TYPE_CART = 2
	start_cartridge = $8000
	end_bank = $A000
	cart_banks = 128
	cart_apaga = $D5FF
	FLAG_16KB = $00
.elseif TYPE_CART = 3
	start_cartridge = $A000
	end_bank = $C000
	cart_banks = 16
	cart_apaga = $D51F
	FLAG_16KB = $00
.elseif TYPE_CART = 4
	start_cartridge = $A000
	end_bank = $C000
	cart_banks = 16
	cart_apaga = $D5FF
	FLAG_16KB = $00
.elseif TYPE_CART = 5
	start_cartridge = $8000
	end_bank = $C000
	cart_banks = 8
	cart_apaga = $D5FF
	FLAG_16KB = $01
.elseif TYPE_CART = 6
	start_cartridge = $8000
	end_bank = $C000
	cart_banks = 64
	cart_apaga = $D5FF
	FLAG_16KB = $01
.else
	.error "Cartridge type not defined"
.endif

//Parameters to init the loader

start_loader = $0400
start_loader2 = $cc00		//Dungeon loader.
start_cart_io =$cd00		//Cartridge IO routines.
start_init2 = $480
start_exit = $100

// Page zero parameters
BUFRLO 	= $32
BUFRHI 	= $33
AUX1	= $34
AUX2	= $35
c_bank	= $36

//Starting the cartridge

	org start_cartridge
	lmb #$00	//BANK 0

// First, copy the loader routine and get back to the OS. You can do whatever you want to initialize.
Copy_init
.proc init
// 1st stage: checks OPTION and puts the first disk loader into memory.
	mva #$ff portb
	mva #$01 basicf
	lda consol
	and #$04	//OPTION??
	bne NO_OPTION	//NO!
	CopyMemory Copy_Exit,start_exit,(.len Exit)
	jmp exit
NO_OPTION
	clc
	rts
.endp
.proc init1
	CopyMemory Copy_loader, start_loader,(.len loader)	//Copy loader to the desired address in the parameters.
	mva #$00 loader.flag	//Avoid turning the cartridge off
	mwa #$600 dbuflo	
	mwa #$01  daux1
	mva #$52  dcomnd
	jsr loader		//Read 1st sector
	mwa #loader $633	//Patch SIO call
	mwa #cont $63e		//Patch final instruction
	mva #$70 $62e		//Patch address for DL.
	jmp $606
cont
	lda #$70
	sta $7005	//Rellocating credits screen from $b000 to $7000
	sta $7019	
	sta $7020
	sta $703f
	sta $707e
	lda #$7d
	sta $700f
	sta $7067	//Done!
	mwa #loader $7046	//Patch SIO call

	mwa #$600 $7001		//Patching "ahoy!" message so it doesn't appear somewhere else.
	mva #$4c $7024		//Once loaded the screen, go to 'CONT2'
	mwa #cont2 $7025	//Done!
	jmp $7000		//Go credits screen!
cont2
	mva #$ff loader.flag	//Resume turning the cartridge off after reading the cartridge
	CopyMemory Copy_init2, init2, (.len init2)		//Copy second init routine if necessary.
	mva #$00 $7063
	sta $7068
	mva #$60 $7064
	sta $7069
	CopyMemory init2.dl_patch,$707b, (init2.dl_end-init2.dl_patch+1)
	lda rtclock
loop0	cmp rtclock
	beq loop0


	CopyMemory $c000,$800,$1000				//Copy OS to RAM
	CopyMemory $d800,$1800,$2800
	lda #$20						//Avoid flashing screen!
	sta chbas						//while copying OS to RAM.
	sta chbase						//Done!
	lda rtclock						//Wait 1 frame
loop1	cmp rtclock
	beq loop1						//Done!

	sei
	mva #$00 nmien						//disabling IRQs, NMIs
	mva #$fe portb						//Enabling upper RAM.
	CopyMemory $800,$c000,$1000				//Put OS on upper RAM
	CopyMemory $1800,$d800,$2800
	lda #$e0						//Restore font to original address
	sta chbas
	sta chbase
	mva #$40 nmien						//Restore NMIs
	CopyMemory Copy_loader2, start_loader2,(.len loader2)
	cli							//Restore IRQs
	jmp init2						//Go to second part!
.endp
Copy_exit		//exit to boot routine.
.proc exit, start_exit
	sei
	lda #$00
	sta nmien	//disable NMIs
	lda #$ff
	sta basicf	//Turn basic off
	sta cart_apaga	//Turn cartridge off
	lda trig3
	sta gintlk	//Avoid the computer to hang up.
	lda #$40
	sta nmien	//Restoring NMIs
	lda #$01
	sta $BFFD	//Boot disk! It's a trick to not to use illegal routines
	cli		//Restore IRQs
	rts		//Go to BOOT!
.endp

Copy_init2
.proc init2, start_init2
	sei			//Turn IRQs off
	mva #$00 nmien		//Turn NMIs off
	ldx #$ff
	txs			//Reset stack pointer.
	stx cart_apaga		//Turn the cartridge off.
	mva trig3 gintlk	//Avoid hanging up
	mva #$40 nmien		//Restore NMIs
	cli			//Restore IRQs
	
	mwa #cont2 $7041	//Patch final instruction
	jmp $702A		//GO!
cont2
	lda #$22
	sta sdmctl
	sta dmactl		//Turn screen on
	mwa #loader $1e43	//Patch SIO call 1
	mwa #loader $1e7d	//Patch SIO call 2
	mwa #cont3 $1e7a	//Patch final instruction
	jmp $2000		//Execute intro!!
cont3
	lda #$20		//NO "48K" display!!
	sta $818f
	sta $8190
	sta $8191		//Done!
	mva #$4c $81a4		//Skip
	mwa #$8221 $81a5	//RAM size detection, Force 48K RAM.

//TO DO: patch custom SIO command to $CC00
//What to know:
//	$230 = SIO drive ID ($31-$34)
//	$231 = SIO command ($52 read, $53 status, $50 put, $22 format)
//	$232 = SIO aux1 (sector number lo byte)
//	$233 = SIO aux2 (sector number hi byte)

	mwa #loader2 $24a1
	mva #$00 $80e1		//Just detect D1: We won't use D2:,D3: or D4:
//	lda #$4c		//NOPs to force detection on D1:
//	sta $80e0
//	mwa #$810d $80e1
	lda #$ea		//Store NOPs
	sta $2893		//Forces no checksum
	sta $2894		//Forces no checksum
	sta $288b		//Forces no checksum
	sta $288c		//Forces no checksum
	sta $810b
	sta $810c		//Don't check D1: a second time.
	ldx #$00
	stx $24f		//Virtual D2: enabled!	
	stx $251		//Virtual D4: enabled!
	dex
	stx $250		//D3: disabled 
	mva #$34 $810e		//Use D4: as main drive
	jmp $807e		//Go to the game!

dl_patch
	.by $70,$42
	.word dl_text
	.by $02
	.by $41
	.word $7060
dl_end
dl_text
	.sb "        Cartridge conversion 2020       "
	.sb "   Guillermo Fuenzalida & Mark Keates   "
.endp

Copy_init3

// Now, we put the loader
Copy_Loader
.proc	loader , start_loader
	sei		// No IRQs!
	lda #$00	
	sta nmien	// No NMIs!
	lda dbuflo	// Take LSB of the address to store
	sta bufrlo	// Store it on Page Zero!
	lda dbufhi	// Take MSB of the addres to store
	sta bufrhi	// Store it!
	sec		// Let's substract 1
	lda daux1	// To the sector number!
	sbc #$01
	sta aux1	// Store it!
	lda daux2	// Take MSB of the sector to read
	sbc #$00	// Make sure we store it
	sta aux2	// on page zero!
	clc		// Clear the carry.
	lda aux1	// Take new sector number
	pha		// save it!
	.if FLAG_16KB = 0
		and #$c0	// Take bits 6 and 7
	.else
		and #$80	//In case of 16 kb banks just take bit 7

	.endif
	:6 lsr		// Move it to bit 0 and 1!
	.if FLAG_16KB = 1
		lsr	// Or bit 7 to 0 in case of 16kb banks
	.endif
	sta aux1	// Store it!
	lda aux2	// Take MSB of the sector.
	asl		// Move 2 bits to the left! Bits 0 and 1 are zero 
	.if FLAG_16KB = 0 //Or 1 bit if it's a Megacart
		asl		// Done!
	.endif
	ora aux1	// Put bits 0 and 1 on from the previous calculation 
	clc		// Preparing to add 1
parameter=*+1		// IMPORTANT: the parameter sets the initial side from the disk. Originally, 1
	adc #$01	// Add it!
	sta c_bank	// Store cartridge bank!
	pla		// take previous LSB of the sector number.
	.if FLAG_16KB = 0
		and #$3F	// Take bits from 0 to 5. Bits 6 and 7 were previously taken to calculate the cartridge bank.
	.else
		and #$7f	// Take bits from 0 to 6. Bit 7 was previously taken to calculate the cartridge bank
	.endif
	lsr		// Shift bit 0 to carry flag. That way, we'll know if the LSB to read on the cartridge is $00 or $80
	ora #>start_cartridge	// Establish the initial address from the cartridge
	sta aux2	// Store it as MSB from the address to read from the cartridge
	lda #$00	// Taking carry
	ror		// To determine if LSB is $00 or $80
	sta aux1	// Save it!
	ldy #$7F	// Number of bytes to read from cartridge (128)
loop
	lda c_bank	// First, we take the cartridge bank calculated
	tax		// Transfer to register X
	sta $d500,x	// And save to the cartridge control area. This way I can use Data bus or address bus bank-switching methods 
	lda (aux1),y	// Read the byte from the cartridge
	pha		// Store it before turning off the cartridge
	lda #$FF	// Let's turn the cartridge off
flag = *+1
	sta cart_apaga	// Done!
	pla		// Recover byte reading
	sta (bufrlo),y	// Store it to the final address
	dey		// Are we done with the byte copying?
	bpl loop	// Not yet
	lda #$c0	// Ending the cartridge reading process. Now we recover the computer status
	sta nmien	// Recover NMIs
	cli		// Recover IRQs
	ldy #$01	// All done without errors
	sty dstats	// Save it to DSTATS!
	rts		// BYE!!
fin_loader
.endp

Copy_loader2
.proc	loader2 , start_loader2

;aux1 = $02
;aux2 = aux1+1
drivenum =$230
drivecommand =$231
driveseclo =$232
drivesechi =$233
status1 = $23d
status2 = $246
buffer = $100
backupbuffer =$cff0

//	lda drivecommand
//	cmp #$53 	//status command?
//	beq go_drive1
	lda drivenum
	cmp #$31	//Is it drive 1?
	jne no_drive1	//No!
	jsr $204e
	bpl end_drive1
	php
	lda drivecommand
	cmp #$53
	bne no_status
	plp
	rts
no_status
	inc drivenum
	plp
	jmp loader2
end_drive1
	rts
no_drive1
	cmp #$34	//Is virtual D4: drive?
	jne drive2	//No! It's the character disk.
//	beq drive4
//	jmp $204e	// Use the disk drive!!!
drive4	
	sei		// No IRQs!
	lda #$00	
	sta nmien	// No NMIs!

	sec		// Let's substract 1
	lda driveseclo	// To the sector number!
	sbc #$01
	sta d4_aux1	// Store it!
	lda drivesechi	// Take MSB of the sector to read
	sbc #$00	// Make sure we store it
	sta d4_aux2	// on page zero!
	clc		// Clear the carry.
	lda d4_aux1	// Take new sector number
	pha		// save it!
	.if FLAG_16KB = 0
		and #$c0	// Take bits 6 and 7
	.else
		and #$80	//In case of 16 kb banks just take bit 7

	.endif
	:6 lsr		// Move it to bit 0 and 1!
	.if FLAG_16KB = 1
		lsr	// Or bit 7 to 0 in case of 16kb banks
	.endif
	sta d4_aux1	// Store it!
	lda d4_aux2	// Take MSB of the sector.
	asl		// Move 2 bits to the left! Bits 0 and 1 are zero 
	.if FLAG_16KB = 0 //Or 1 bit if it's a Megacart
		asl		// Done!
	.endif
	ora d4_aux1	// Put bits 0 and 1 on from the previous calculation 
	clc		// Preparing to add 1
d4_parameter=*+1	// IMPORTANT: the parameter sets the initial side from the disk. Originally, 1
	adc #$01	// Add it!
	sta d4_c_bank	// Store cartridge bank!
	pla		// take previous LSB of the sector number.
	.if FLAG_16KB = 0
		and #$3F	// Take bits from 0 to 5. Bits 6 and 7 were previously taken to calculate the cartridge bank.
	.else
		and #$7f	// Take bits from 0 to 6. Bit 7 was previously taken to calculate the cartridge bank
	.endif
	lsr		// Shift bit 0 to carry flag. That way, we'll know if the LSB to read on the cartridge is $00 or $80
	ora #>start_cartridge	// Establish the initial address from the cartridge
	sta d4_aux2	// Store it as MSB from the address to read from the cartridge
	lda #$00	// Taking carry
	ror		// To determine if LSB is $00 or $80
	sta d4_aux1	// Save it!
	ldy #$7F	// Number of bytes to read from cartridge (128)
d4_ldacbank
	lda #$FF	// First, we take the cartridge bank calculated
d4_c_bank = d4_ldacbank+1
	tax		// Transfer to register X
	sta $d500,x	// And save to the cartridge control area. This way I can use Data bus or address bus bank-switching methods 
d4_loop
	lda $FFFF,y	// Read the byte from the cartridge
d4_aux1 = d4_loop+1
d4_aux2 = d4_aux1+1
	sta buffer,y	// Store it to the final address
	dey		// Are we done with the byte copying?
	bpl d4_loop	// Not yet
	lda #$ff
	sta cart_apaga
	lda #$c0	// Ending the cartridge reading process. Now we recover the computer status
	sta nmien	// Recover NMIs
	cli		// Recover IRQs


//Let's patch the SPACE BAR for character disk
	lda $7ebe
	cmp #$30
	bne d4_2nd
	lda $7ebf
	cmp #$fb
	bne d4_2nd
	
//Let's patch the SPACE BAR from the first menu!	
	lda #$4c
	sta $7ebe
	lda #$cb
	sta $7ebf
	lda #$7e
	sta $7ec0
	
d4_2nd
	lda $762f
	cmp #$30
	bne d4_3rd
	lda $7630
	cmp #$fb
	bne d4_3rd
//Let's patch the SPACE BAR from saving the character during the game
	lda #$ea
	sta $762f
	sta $7630
d4_3rd
	lda $7608
	cmp #$20	//JSR?
	bne d4_4th	//No!
	lda #$18
	cmp $7609
	bne d4_4th
	cmp $760a
	bne d4_4th
	mwa #put_31 $7609
d4_4th
	lda $8404
	cmp #$A9	//LDA?
	bne d4_5th
	lda $8405
	cmp #$31	//Drive 1?
	bne d4_5th	//Nope!
	mwa #char_format $8402
d4_5th
	lda $77bf
	cmp #$a9
	bne d4_end
	lda $77c0
	cmp #$57
	bne d4_end
	lda #$50
	sta $776c
	sta $77a3
	sta $77c0

d4_end
	ldy #$01	// All done without errors
	sty status1	// Save it to DSTATS!
	sty status2
	rts		// BYE!!

drive2
	lda drivecommand
	cmp #$21	//Is it a format command?
	jne no_format	//No! It's a write or read

//Let's format it! Will erase all sectors for D1:
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
	sta cart_apaga
	ldy #$01	// All done without errors
	sty status1	// Save it to DSTATS!
	sty status2
	rts		//Formatting successful
no_format


//	rts		//NEED TO BE REMOVED!!!!


	pha		//Store command for a while
//Now will check if it's the initial sector from the block of the disk.
//In case it's the initial one, it erases the entire sector
	ldx #$00	//Counter
	stx sector_selected
	ldy #$01	//Start with no detection!!
d2_loop1
	lda drivesechi		//Take MSB of the sector
	cmp sec_table+1,x	//Is it from the table?
	beq d2_loop1_0 
	bcc d2_loop2		//NO! It's higher. Don't count this.
	bne d2_loop1_2		//No! It's lower. Take x to the sector selected.

d2_loop1_0
	lda driveseclo		//It's equal. Now let's get LSB of the sector.
	cmp sec_table,x		//Is it from the table?
	beq d2_loop1_1
	bcc d2_loop2		//No, it's higher. next sector!
	bne d2_loop1_2		//No, it's lower. Take x to the sector selected.
d2_loop1_1
	dey			//It's the same! Put Y = 0.
d2_loop1_2
	stx sector_selected
d2_loop2
	inx
	inx
	cpx #$0a	//All 5 sectors checked?
	bne d2_loop1	//Not yet!
	ldx sector_selected
	lda offset_table,x	//Take the offset
	sta sec_offset		//Store it!
	lda offset_table+1,x	//MSB offset
	sta sec_offset+1	//Store it!
	lsr sector_selected
	ldx sector_selected
	lda bank_table,x
	sta d2_parameter	//Change initial bank to take

drive2_put
	sei		// No IRQs!
	lda #$00	
	sta nmien	// No NMIs!

	sec		// Let's substract 1
	lda driveseclo	// To the sector number!
	sbc sec_offset
	sta d2_read_aux1	// Store it!
	lda drivesechi	// Take MSB of the sector to read
	sbc sec_offset+1	// Make sure we store it
	sta d2_read_aux2	// on page zero!
	clc		// Clear the carry.
	lda d2_read_aux1	// Take new sector number
	pha		// save it!
	and #$c0	// Take bits 6 and 7
	:6 lsr		// Move it to bit 0 and 1!
	sta d2_read_aux1	// Store it!
	lda d2_read_aux2	// Take MSB of the sector.
	asl		// Move 2 bits to the left! Bits 0 and 1 are zero 
	asl		// Done!
	ora d2_read_aux1	// Put bits 0 and 1 on from the previous calculation 
	clc		// Preparing to add 1
d2_parameter=*+1	// IMPORTANT: the parameter sets the initial side from the disk. Originally, 1
	adc #$01	// Add it!
	sta d2_read_c_bank	// Store cartridge bank!
	sta d2_write_c_bank
	pla		// take previous LSB of the sector number.
	and #$3F	// Take bits from 0 to 5. Bits 6 and 7 were previously taken to calculate the cartridge bank.
	lsr		// Shift bit 0 to carry flag. That way, we'll know if the LSB to read on the cartridge is $00 or $80
	ora #>start_cartridge	// Establish the initial address from the cartridge
	sta d2_read_aux2	// Store it as MSB from the address to read from the cartridge
	sta d2_write_aux2
	lda #$00	// Taking carry
	ror		// To determine if LSB is $00 or $80
	sta d2_read_aux1	// Save it!
	sta d2_write_aux1

//Now we start to copy the bytes (read/write)

	ldx #$7f
	pla		//Restore command
	cmp #$57	//Write?
	beq d2_write
	cmp #$50
	beq d2_write
d2_read_ldacbank
	lda #$ff
d2_read_c_bank =d2_read_ldacbank+1
	tay
	sta $d500,y
d2_read_loop
	lda $FFFF,x	// Read the byte from the cartridge
d2_read_aux1 = d2_read_loop+1
d2_read_aux2 = d2_read_aux1+1
	sta buffer,x	// Store it to the final address
	dex		// Are we done with the byte copying?
	bpl d2_read_loop	// Not yet
	bmi d2_end	//Let's finish

d2_write
	tya		//Is is first sector?
	bne d2_write_cont	//Nope, let's continue
	lda sector_selected	
	clc
	adc #$0a
	jsr erasebk		//Erase the bank and start writing!
d2_write_cont

d2_write_loop
d2_write_ldacbank
	lda #$ff
d2_write_c_bank =d2_write_ldacbank+1
	jsr enable_write
	lda buffer,x
d2_write_sta	
	sta $FFFF,x
d2_write_aux1 = d2_write_sta+1
d2_write_aux2 = d2_write_aux1+1
	dex
	bpl d2_write_loop

d2_end
	lda #$ff
	sta cart_apaga
	lda #$c0	// Ending the cartridge reading process. Now we recover the computer status
	sta nmien	// Recover NMIs
	cli		// Recover IRQs
	ldy #$01	// All done without errors
	sty status1	// Save it to DSTATS!
	sty status2
	rts		// BYE!!

put_31
	lda #$31	//Put D1: first
	sta drivenum	
	jmp $1818	//Go!

char_format
	jsr $1827
	ldx #$31
	lda $24e
	bpl char_format_si31
	inx
char_format_si31
	stx $8405	//Store drive number
	lda #$4c
	sta $8421
	mwa #char_format_error $8422	//Patch format error.
	rts

char_format_error
	lda #$fe
	sta $16
	lda $230
	cmp #$31
	beq char_format_error_31
	jmp $8425
char_format_error_31
	inc $230
	jmp $8409

sec_table		//List of initial sectors to write on 

//This table marks the sectors we'll take into account to erase the entire sector.
//That is, the initial disk sector from we'll erase.
	.word $0002,$0003,$00bb,$0173,$022b
offset_table
	.word $0001,$0003,$00bb,$0173,$022b
sec_offset
	.word $0000	//Sector offset to substract from original cartridge sector.
//bank_table		//List of initial bank per sector. The first 10 sectors are for D4:. Banks $0a-$0f to D1:
	.by $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
bank_table
	.by $50,$58,$60,$68,$70,$78
sector_selected
	.by $00		//By default, sector 0
bank_selected
	.by $00		//By default, bank 0

//Code for managing the Atarimax flash cartridge. Please take note that this routine
//is implemented just for the second chip.
//To add functionality to the first chip, you must change the wr5555, wr2aaa and add a chip mask routine.


fcode
setsec
	and #$0F	//Only $00-$0F allowed
	clc		//Just to not set bit 7 to 1 accidentally
	rol		//*2
	rol		//*4
	rol		//*8
	tax
	sta $d500,x	//Change bank!
	rts

wr5555
	sta $d542	//Setting third bank from second chip.
	sta $b555	//Store on $5555!
	rts

cmd_unlock
	lda #$AA	//Store $aa on $5555
	jsr wr5555	//Do it!
	lda #$55	//Store $55 on $2aaa

wr2AAA
	sta $d541	//Setting second bank from second chip.
	sta $aaaa	//Store on $2aaa
	rts


//Enable_write:
//enables the 29F040 chip to write one byte.
//It only enables one byte. You have to call this routine for every byte you want to write.
enable_write
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

//Enable read: not needed for now. Just to have it.
enable_read
	pha
	jsr cmd_unlock
	lda #$f0
	jsr wr5555
	jmp enable_write_cont
erasebk
	stx temp_x
	pha
	jsr cmd_unlock		//First two cycles!
	lda #$80
	jsr wr5555		//Third cycle!
	jsr cmd_unlock		//Fourth and fifth cycles!
	pla
	jsr setsec
	lda #$30		//Sixth and final cycle!
	sta start_cartridge	//Erase!
	
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
	ldx temp_x
	rts
pollsame
	.by $00
temp_x	.by $00
	
//	icl "fcode.s"	//Courtesy from Wrathchild at Atariage. Thanks!

chipmask
	.byte $00
final_greeting
	.sb "Cartridge version 2020 by Guillermo Fuenzalida, based on the works from Mark Keates"
fin_loader
.endp

	opt f+		//start filling!
	
	org end_bank-6-3	//// Put it into the end
	jmp init
	.word init1		// Second init address first.
	.byte $00,$04		// Parameters to not to call to Disk.
	.word init		// First init address
	opt f-			// No more filling!

// Bank 2
	org start_cartridge
	lmb #$01
start_atr
	ins "ARD_WIP_rel3.atr",16
end_atr

//Now we fill till we got cartridge size

total_bytes	= end_atr - start_atr		//Total bytes of ATR
bank_size	= end_bank - start_cartridge

banks_used = ((total_bytes+(bank_size-1))/bank_size)+1
filler_banks = cart_banks - banks_used
filler_bytes = bank_size*(banks_used-1)-total_bytes

; Llenamos los bytes

.if filler_banks > 0
	.sav filler_bytes
.else
	.sav filler_bytes - 6 -8
.endif

end_file


.if filler_banks > 0
	.if filler_banks > 1
		.rept filler_banks-1
			.sav bank_size
		.endr
	.endif
	.sav bank_size-6-8
.endif


	lda #$00
	sta $d500
	jmp init
	.word init1
	.byte $00,$04
	.word $bff2
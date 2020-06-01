// ****************************************************
// *                                                  *
// * Generic BOOT Cartridge routine                   *
// *                                                  *
// *     (C) Guillermo Fuenzalida (Willysoft)         *
// *                                                  *
// ****************************************************




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
start_loader2 = $cc00		//Dungeon loader
start_init2 = $480


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
// 1st stage
	mva #$ff portb
	mva #$01 basicf
	CopyMemory Copy_loader, start_loader,(.len loader)	//Copy loader to the desired address in the parameters.
	CopyMemory Copy_init2, init2, (.len init2)		//Copy second init routine if necessary.
	sei
	mva #$00 nmien
	CopyMemory $c000,$600,$1000				//Copy OS to RAM
	CopyMemory $d800,$1600,$2800
	mva #$fe portb
	CopyMemory $600,$c000,$1000				//Put OS on upper RAM
	CopyMemory $1600,$d800,$2800
	mva #$40 nmien
	CopyMemory Copy_loader2, start_loader2,(.len loader2)
	cli
	clc	// No errors!
	rts	// Done
.endp
Copy_init2
.proc init2, start_init2
	ldx #$ff
	txs
	stx cart_apaga
	mva trig3 gintlk
	mwa #$600 dbuflo
	mwa #$01  daux1
	mva #$52  dcomnd
	jsr loader		//Read 1st sector
	mwa #loader $633	//Patch SIO call
	mwa #cont $63e		//Patch final instruction
	mva #$90 $62e
	jmp $606
cont
;	lda #$00
;	sta sdmctl
;	sta dmactl		//Turn screen off
	lda #$90
	sta $9005
	sta $9019
	sta $9020
	sta $903f
	sta $907e
	lda #$9d
	sta $900f
	sta $9067
	mwa #loader $9046	//Patch SIO call
	mwa #cont2 $9041	//Patch final instruction
	jmp $9000		//GO!
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
	mwa #$8221 $81a5	//RAM size detection

//TO DO: patch custom SIO command to $CC00
//What to know:
//	$230 = SIO drive ID ($31-$34)
//	$231 = SIO command ($52 read, $53 status, $50 put, $22 format)
//	$232 = SIO aux1 (sector number lo byte)
//	$233 = SIO aux2 (sector number hi byte)
// Pending: where to locate the buffer ($CD00? or similar.)

	mwa #loader2 $24a1
	mva #$02 $80e1		//Skip Virtual D4: detection
	lda #$ea		//NOPs to force detection on D1:
	sta $810b
	sta $810c
	sta $2893		//Forces no checksum
	sta $2894		//Forces no checksum
	sta $288b		//Forces no checksum
	sta $288c		//Forces no checksum
	mva #$00 $251		//Virtual D4: enabled! 
	mva #$34 $810e
	jmp $807e		//Go to the game!
.endp

Copy_init3

// Now, we put the loader
Copy_Loader
.proc	loader , start_loader
	sei		// No IRQs!
	lda nmien	// Save NMIEN
	pha		// Store it
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
	sta cart_apaga	// Done!
	pla		// Recover byte reading
	sta (bufrlo),y	// Store it to the final address
	dey		// Are we done with the byte copying?
	bpl loop	// Not yet
	pla		// Ending the cartridge reading process. Now we recover the computer status
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
	lda drivenum
	cmp #$34
	beq si_drive
	jmp $204e	// Utilice el disco!!!

si_drive	
	sei		// No IRQs!
	lda nmien	// Save NMIEN
	pha		// Store it
	lda #$00	
	sta nmien	// No NMIs!

	sec		// Let's substract 1
	lda driveseclo	// To the sector number!
	sbc #$01
	sta aux1	// Store it!
	lda drivesechi	// Take MSB of the sector to read
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
ldacbank
	lda #$FF	// First, we take the cartridge bank calculated
c_bank = ldacbank+1
	tax		// Transfer to register X
	sta $d500,x	// And save to the cartridge control area. This way I can use Data bus or address bus bank-switching methods 
loop
	lda $FFFF,y	// Read the byte from the cartridge
aux1 = loop+1
aux2 = aux1+1
	sta buffer,y	// Store it to the final address
	dey		// Are we done with the byte copying?
	bpl loop	// Not yet
	pla		// Ending the cartridge reading process. Now we recover the computer status
	sta nmien	// Recover NMIs
	cli		// Recover IRQs
	ldy #$01	// All done without errors
	sty status1	// Save it to DSTATS!
	sty status2
	rts		// BYE!!
fin_loader
.endp

	opt f+		//start filling!
	
	org end_bank-6-3	//// Put it into the end
	jmp init
	.word init2		// Second init address first.
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
	.word start_loader
	.byte $00,$04
	.word $bff2
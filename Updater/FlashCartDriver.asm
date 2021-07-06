
//Flash cartridge driver
//2021 Guillermo Fuenzalida
//


start_cartridge = $a000
end_cartridge	= $c000
num_tries	= $10

//	org $2000

chip_select
	.by $00	

//SetSector: selects the first bank of the sector selected.
//Parameter: A= number of sector ($00-$0F)	
.proc SetSector
	and #$0F	//Only $00-$0F allowed
	clc		//Just to not set bit 7 to 1 accidentally
	asl		//*2
	asl		//*4
	asl		//*8
	tax
	sta $d500,x	//Change bank!
	rts
.endp


//Command_Unlock: first two steps to unlock the flash chip.
.proc Command_Unlock
	lda #$AA
	jsr Write_5555
	lda #$55
	jmp Write_2AAA

.endp

.proc CheckID
	stx save_x+1
	jsr Command_Unlock
	lda #$90
	jsr Write_5555
	ldx chip_select
	sta $d500,x
	lda $a000
	ora $a001
	pha
	jsr Command_Unlock
	lda #$f0
	jsr Write_5555
	ldx #$00
loop
	inx
	bne loop

save_x
	ldx #$00
	pla
	rts
.endp

//is39F: looks if chipset if sst39sf040.
//Output: Carry set = is sst39sf040, carry clear = none of this.
.proc is39F
	jsr CheckID
	cmp #$bf
	bne no_sst
	sec
	rts
no_sst
	clc
	rts
.endp


// VerifyBlankSector: verifies if sector is erased ($FF)
// Parameter A=Sector number (0 to 15)
// Output: Carry clear = OK, Carry Set = Error
.proc VerifyBlankSector

bank	= $d500
pt_1	= $cb
pt_2	= $cd

	pha
	stx save_x+1	//save X register
	sty save_y+1	//save Y register
	clc				//By default, we are ok
	ldy #$00
	:3 asl 			//*8
	tax				//Save bank on X
	mva #$08 count

loop1
	sta bank,x		//Activate bank
	mwa #start_cartridge loop2+1	//init the loop with start_cartridge
loop2
	lda $ffff,y
	cmp #$ff
	bne error
	iny
	bne loop2
	inc loop2+2
	lda loop2+2
	cmp #>end_cartridge
	bne loop2
	inx
	dec count
	bne loop1
	clc
	bcc end_loop
error
	sec
end_loop

save_x
	ldx #$00
save_y
	ldy #$00
	pla
end_verify
	rts
count
	.by $00
.endp


//Erase_Cartridge: erases an entire cartridge
//Parameter: none

.proc Erase_cartridge
	pha
	lda #$00
	sta chip_select
begin_erase
	jsr Command_Unlock
	lda #$80
	jsr Write_5555
	lda #$aa
	jsr Write_5555
	lda #$55
	jsr Write_2aaa
	lda #$10
	jsr Write_5555
	jsr pollwrite
	lda chip_select
	bne end_erase
	lda #$40
	sta chip_select
	bne begin_erase
end_erase
	lda #$00
	sta chip_select
	pla
	rts
.endp

//EraseSector: erases sector of 64kbytes
//Parameter: A=sector to erase 
.proc Erase_Sector
	pha		//Save sector
	and #$08
	beq erase2
	lda #$40
erase2
	sta chip_select
	
	jsr is39f
	bcs erase3

	jsr Common

	pla		//Recover sector
	pha
	jsr SetSector
	
	
	lda #$30
	sta start_cartridge	//6th cycle

//Erase command sent. Now let's wait until it's finished.

	mva #num_tries sec_count
	
	pla
	pha
	
loop
	
	jsr pollwrite
	
	pla
	pha
	
	jsr VerifyBlankSector
	bcc exit
	dec sec_count
	bne loop
	sec
exit
	pla
	rts

erase3
	stx save_x+1
	sty save_y+1
	mva #$08 sec_count

	pla
	pha
	asl
	asl
	asl
	sta bank_number

loop2	
	jsr Common
	ldx bank_number
	sta $d500,x
	lda #$30
	sta start_cartridge
	jsr pollwrite
	
	jsr Common
	ldx bank_number
	sta $d500,x
	lda #$30
	sta start_cartridge+$1000
	jsr pollwrite2
	inc bank_number
	dec sec_count
	bne loop2
	pla
	pha
	jsr VerifyBlankSector
save_x
	ldx #$00
save_y
	ldy #$00	
	pla
	rts

Common
	jsr Command_Unlock //1st and 2nd cycle
	
	lda #$80
	jsr Write_5555	//3rd cycle
	lda #$aa
	jsr Write_5555	//4th cycle
	lda #$55
	jmp Write_2aaa	//5th cycle
	

sec_count
	.by $00
bank_number
	.by $00

.endp


//PollWrite: waits until the sector data stopped changing.
.proc pollwrite

byte_address = start_cartridge

	pha
again
	mva #$00 pollsame
loop
	lda byte_address
	cmp byte_address
	bne again
	cmp byte_address
	bne again
	cmp byte_address
	bne again
	inc pollsame
	bne loop
	pla
	rts
pollsame
	.by $00
.endp

.proc pollwrite2

byte_address = start_cartridge+$1000

	pha
again
	mva #$00 pollsame
loop
	lda byte_address
	cmp byte_address
	bne again
	cmp byte_address
	bne again
	cmp byte_address
	bne again
	inc pollsame
	bne loop
	pla
	rts
pollsame
	.by $00
.endp

//Write_2aaa: writes value at $2aaa on the respective chip selected.
//Parameter: A=value to write
.proc write_2aaa
	bit chip_select
	bvc step2
	sta $d541
	sta $aaaa
	rts
step2
	sta $d501
	sta $aaaa
	rts
.endp

.proc write_5555
	bit chip_select
	bvc step2
	sta $d542
	sta $b555
	rts
step2
	sta $d502
	sta $b555
	rts
.endp

//Enable Write: enables one byte to write on the cartridge
//Parameter: A = bank to write.

.proc Enable_Write
	stx save_x+1
	pha					//Save bank to write on.
	and #$7f			//Avoid error on using banks >=128
	:3 lsr				//divide by 8
	and #$08
	beq erase2
	lda #$40
erase2
	sta chip_select
	jsr Command_Unlock	//1st and 2nd cycle
	lda #$a0
	jsr Write_5555		//3rd cycle
	pla					//Recover bank
	tax
	sta $d500,x
save_x
	ldx #$00
	rts
.endp


//Erase_Sectors: erase a number of sectors
//Parameters: X=Initial sector, Y=number of sectors to erase.
.proc Erase_Sectors
	pha		//Save accumulator
	stx sector
	sty num_sectors
loop
	lda sector
	jsr Erase_Sector
	inc sector
	dec num_sectors
	bne loop
	pla
	rts
sector
	.by $00
num_sectors
	.by $00
.endp

//VerifyBlankSectors: Verify status blank of sector
//Parameters: X=Initial sector, Y=number of sectors to verify.
//Output: Carry clear: OK, Carry Set = Error
.proc VerifyBlankSectors
	pha
loop
	txa
	jsr VerifyBlankSector
	bcs end_verify
	inx
	dey
	bne loop
	clc
end_verify
	pla
	rts
	
.endp
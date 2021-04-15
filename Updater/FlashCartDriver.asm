


start_cartridge = $a000
end_cartridge	= $c000


//	org $2000
	

//SetSector: selects the first bank of the sector selected.
//Parameter: A= number of sector ($00-$0F)	

chip_select
	.by $00

.proc SetSector
	and #$0F	//Only $00-$0F allowed
	clc		//Just to not set bit 7 to 1 accidentally
	rol		//*2
	rol		//*4
	rol		//*8
	tax
	sta $d500,x	//Change bank!
	rts
.endp

.proc Command_Unlock
	lda #$AA
	jsr Write_5555
	lda #$55
	jmp Write_2AAA

.endp


// VerifyBlankSector: verifies if sector is erased ($FF)
// Parameter A=Sector number (0 to 15)
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
	jsr Command_Unlock //1st and 2nd cycle
	
	lda #$80
	jsr Write_5555	//3rd cycle
	lda #$aa
	jsr Write_5555	//4th cycle
	lda #$55
	jsr Write_2aaa	//5th cycle

	pla		//Recover sector
	pha
	jsr SetSector
	
	
	lda #$30
	sta start_cartridge	//6th cycle

//Erase command sent. Now let's wait until it's finished.
	pla
.endp

.proc pollwrite
	pha
again
	mva #$00 pollsame
loop
	lda start_cartridge
	cmp start_cartridge
	bne again
	cmp start_cartridge
	bne again
	cmp start_cartridge
	bne again
	inc pollsame
	bne loop
//	pla
//	pha
//	jsr VerifyBlankSector
//	bcs pollwrite
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

//VerifyBlankSectors: Verifynumber of sector
//Parameters: X=Initial sector, Y=number of sectors to erase.
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
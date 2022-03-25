.INCLUDE "m128def.inc"
.equ    fclk                = 8000000      ; system clock frequency (for delays)
; register usage
.def    temp                = R16           ; temporary storage
; LCD interface
.equ    lcd_D7_port         = PORTC         ; lcd D7 connection
.equ    lcd_D7_bit          = PORTC7
.equ    lcd_D7_ddr          = DDRC
.equ    lcd_D6_port         = PORTC         ; lcd D6 connection
.equ    lcd_D6_bit          = PORTC6
.equ    lcd_D6_ddr          = DDRC
.equ    lcd_D5_port         = PORTC         ; lcd D5 connection
.equ    lcd_D5_bit          = PORTC5
.equ    lcd_D5_ddr          = DDRC
.equ    lcd_D4_port         = PORTC         ; lcd D4 connection
.equ    lcd_D4_bit          = PORTC4
.equ    lcd_D4_ddr          = DDRC
.equ    lcd_E_port          = PORTB         ; lcd Enable pin
.equ    lcd_E_bit           = PORTB5
.equ    lcd_E_ddr           = DDRB
.equ    lcd_RS_port         = PORTB         ; lcd Register Select pin
.equ    lcd_RS_bit          = PORTB2
.equ    lcd_RS_ddr          = DDRB
; LCD module Lines
.equ    lcd_LineOne         = 0x00          ; line 1
.equ    lcd_LineTwo         = 0x40          ; line 2
; LCD Defined instructions
.equ    lcd_Clear           = 0b00000001    ; ASCII 'space' for all characters
.equ    lcd_Home            = 0b00000010    ; first position on first line
.equ    lcd_EntryMode       = 0b00000110    ; shift cursor from left to right on read/write
.equ    lcd_DisplayOff      = 0b00001000    ; turn display off
.equ    lcd_DisplayOn       = 0b00001100    ; display on, cursor off, don't blink character
.equ    lcd_FunctionReset   = 0b00110000    ; reset the LCD
.equ    lcd_FunctionSet4bit = 0b00101000    ; 4?bit data, 2?line display, 5 x 7 font
.equ    lcd_SetCursor       = 0b10000000    ; set cursor position


.def	num1				= R17
.def	counter				= R20
.def	G					= R18
.equ	ONES				= 0xFF
.equ	ZEROS				= 0x00

.def	quotient			= R21
.def	remainder			= R23

.equ	result1				= 0x0100
.equ	result2				= 0x0200

.CSEG

	ldi     temp,low(RAMEND)
    out     SPL,temp
    ldi     temp,high(RAMEND)
    out     SPH,temp

MAIN:
	call	get_input
	call	CRC_code
	call	LCD

rjmp	MAIN
;___________________________________________________________________________________________CRC code SUBROUTINE___________________________________________________________________________________________
.org 0x005000
CRC_code:
	ldi		R24, 0x30	;'0' input on ascii
	ldi		R25, 0x31	;'1' input on ascii
	ldi		quotient, 0x00
	ldi		R22, 0x00
	ldi		G, 0x98 ;	(000X^4 00X1) --> (0001 0011) but while xor process the 0's are not importnat so, it turns to (1001 1000) 0x98
	ldi		counter, 3 ;max 3 shifts enough while computing
	mov		temp, num1
calc:
	sbrs	temp,7
	call	sh_zero
	cpi		counter, 0
	breq	move
	sbrc	temp,7
	call	sh_pl
	brne	calc
move:
	mov		R26, num1
	ldi		counter, 8
	call	rep			;the input in binary
	ldi		counter, 4
	call	zero		;4_bit zeros before 4-bit remainder
	call	sh_rem		;shift remainder till it starts with 1
	mov		R26, remainder
	CLC
	ldi		counter, 4
	call	pad_rem
	call	conv_to_hex
	ret
sh_zero:
lsl		temp
dec counter
ret
sh_pl:
call	exor	;if the processed input starts with 1 go exor op
dec		counter
ret

exor:
	mov		R22, temp
	andi	R22, 0xF8		;hold the first 5 bits of the input
	eor		R22, G		; xor the data(output) with crc code(r20)
	mov		remainder, R22	;hold the remainder to add result
	andi	temp, 0x07		;(0000 0xxx)
	andi	R22, 0xF8		;(RRRR R000)
	add		remainder, temp	;there may last unused bits on temp
	add		temp, R22
	lsl		temp
	ret
;______________________________________________ save data mem as hex _______________________________________________________
conv_to_hex:
	ldi		ZH,	HIGH(result1)
	ldi		ZL,	LOW(result1)
	mov		temp, R26
	swap	temp
	andi	temp, 0x0F
	call	overeight
	mov		temp, R26
	andi	temp, 0x0F
	sbrc	temp, 3
	call	overeight
	add		temp, R24
	st		Z+, temp
	ldi		R18, ZEROS
	st		Z,	R18
	ret
overeight:
	subi	temp, 0x0A
	brmi	underten
	ldi		R18, 0x41
	add		temp, R18
	st		Z+, temp
	ldi		R18, ZEROS
	st		Z,	R18
	ret
underten:
	mov		temp, R26
	andi	temp, 0x0F
	add		temp, R24
	st		Z+, temp
	ldi		R18, ZEROS
	st		Z,	R18
	ret

; ______________________________________________________ save data mem as binary ________________________________________________
rep:
	ldi		ZH,	HIGH(result2)
	ldi		ZL,	LOW(result2)
eminem:
	sbrc	R26,7
	st		Z+,	R25
	sbrs	R26, 7
	st		Z+,	R24
	ROL		R26
	dec		counter
	brne	eminem
	ret
zero:
	st		Z+, R24
	dec		counter
	brne	zero
	ret
pad_rem:
	sbrs	remainder,7
	st		Z+,	r24
	sbrc	remainder,7
	st		Z+,	r25
	ROL		remainder
	dec		counter
	brne	pad_rem
fin:
	ldi		R20, ZEROS
	st		Z, R20
	ret

sh_rem:
	sbrs	remainder, 7
	lsl		remainder
	brpl	sh_rem
	ret

;___________________________________________________________________________________________INPUT SUBROUTINE___________________________________________________________________________________________

.org	0x007000
get_input:
	ldi		temp,	ZEROS
	out		DDRA,	temp
	in		num1,	PINA
	ret
;_____________________________________________________________________________________LCD_init & display SUBROUTINE___________________________________________________________________________________________

.org    0x0900
LCD:
; configure the microprocessor pins for the data lines
    sbi     lcd_D7_ddr, lcd_D7_bit          ; 4 data lines ? output
    sbi     lcd_D6_ddr, lcd_D6_bit
    sbi     lcd_D5_ddr, lcd_D5_bit
    sbi     lcd_D4_ddr, lcd_D4_bit
; configure the microprocessor pins for the control lines
    sbi     lcd_E_ddr,  lcd_E_bit           ; E line ? output
    sbi     lcd_RS_ddr, lcd_RS_bit          ; RS line ? output
; initialize the LCD controller
    call    lcd_init_4d                     ; initialize the LCD display for a 4?bit interface


	ldi		ZH,	HIGH(result1)
	ldi		ZL,	LOW(result1)
	ldi		temp, lcd_LineOne
	call	lcd_write_string_4d

	ldi		ZH,	HIGH(result2)
	ldi		ZL,	LOW(result2)
	ldi		temp, lcd_LineTwo
	call	lcd_write_string_4d
	ret

;______________________________________________________________________________________________________________________________________________________________________________________
;______________________________________________________________________________________________________________________________________________________________________________________
;______________________________________________________________________________________________________________________________________________________________________________________
;______________________________________________________________________________________________________________________________________________________________________________________

												; ****************************** End of Main Program Code *******************

; ============================== 4?bit LCD Function Calls ======================
; Name:     lcd_init_4d ?? initialize the LCD module for a 4?bit data interface
lcd_init_4d:
; Power?up delay
    ldi     temp, 100                       ; initial 40 mSec delay
    call    delayTx1mS

    cbi     lcd_RS_port, lcd_RS_bit         ; select the Instruction Register (RS low)
    cbi     lcd_E_port, lcd_E_bit           ; make sure E is initially low
; Reset the LCD controller.
    ldi     temp, lcd_FunctionReset         ; first part of reset sequence
    call    lcd_write_4
    ldi     temp, 10                        ; 4.1 mS delay (min)
    call    delayTx1mS
    ldi     temp, lcd_FunctionReset         ; second part of reset sequence
    call    lcd_write_4
    ldi     temp, 200                       ; 100 uS delay (min)
    call    delayTx1uS
    ldi     temp, lcd_FunctionReset         ; third part of reset sequence
    call    lcd_write_4
    ldi     temp, 200                       ; this delay is omitted in the data sheet
    call    delayTx1uS

    ldi     temp, lcd_FunctionSet4bit       ; set 4?bit mode
    call    lcd_write_4
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
; Function Set instruction
    ldi     temp, lcd_FunctionSet4bit       ; set mode, lines, and font
    call    lcd_write_instruction_4d
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS

    ldi     temp, lcd_DisplayOff            ; turn display OFF
    call    lcd_write_instruction_4d
    ldi     temp, 80                        ; 40 uS delay (min)

    call    delayTx1uS
; Clear Display instruction
    ldi     temp, lcd_Clear                 ; clear display RAM
    call    lcd_write_instruction_4d
    ldi     temp, 4                         ; 1.64 mS delay (min)
    call    delayTx1mS
; Entry Mode Set instruction
    ldi     temp, lcd_EntryMode             ; set desired shift characteristics
    call    lcd_write_instruction_4d
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS

    ldi     temp, lcd_DisplayOn             ; turn the display ON
    call    lcd_write_instruction_4d
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
    ret
; ???????????????????????????????????????????????????????????????????????????

lcd_write_string_4d:
; preserve registers
    push    ZH                              ; preserve pointer registers
    push    ZL
; set up the initial DDRAM address
    ori     temp, lcd_SetCursor             ; convert the plain address to a set cursor instruction
    call   lcd_write_instruction_4d         ; set up the first DDRAM address
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
; write the string of characters
lcd_write_string_4d_01:
    ld     temp, Z                       ; get a character
    cpi     temp,  0                        ; check for end of string
    breq    lcd_write_string_4d_02          ; done
; arrive here if this is a valid character
    call    lcd_write_character_4d          ; display the character
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
inc     zl
    rjmp    lcd_write_string_4d_01          ; not done, send another character
; arrive here when all characters in the message have been sent to the LCD module
lcd_write_string_4d_02:

    pop     ZL                              ; restore pointer registers
    pop     ZH
    ret
; ???????????????????????????????????????????????????????????????????????????
; Name:     lcd_write_character_4d

lcd_write_character_4d:
    sbi     lcd_RS_port, lcd_RS_bit         ; select the Data Register (RS high)
    cbi     lcd_E_port, lcd_E_bit           ; make sure E is initially low
    call    lcd_write_4                     ; write the upper 4?bits of the data
    swap    temp                            ; swap high and low nibbles
    call    lcd_write_4                     ; write the lower 4?bits of the data
    ret
; ???????????????????????????????????????????????????????????????????????????
; Name:     lcd_write_instruction_4d ?? Send a byte of information to the LCD instruction register
lcd_write_instruction_4d:
    cbi     lcd_RS_port, lcd_RS_bit         ; select the Instruction Register (RS low)
    cbi     lcd_E_port, lcd_E_bit           ; make sure E is initially low
    call    lcd_write_4                     ; write the upper 4?bits of the instruction
    swap    temp                            ; swap high and low nibbles
    call    lcd_write_4                     ; write the lower 4?bits of the instruction
    ret
; ???????????????????????????????????????????????????????????????????????????
; Name:     lcd_write_4 Send 4?bits of information to the LCD module
lcd_write_4:
; set up D7
    sbi     lcd_D7_port, lcd_D7_bit         ; assume that the D7 data is '1'
    sbrs    temp, 7                         ; check the actual data value
    cbi     lcd_D7_port, lcd_D7_bit         ; arrive here only if the data was actually '0'
; set up D6
    sbi     lcd_D6_port, lcd_D6_bit         ; repeat for each data bit
    sbrs    temp, 6
    cbi     lcd_D6_port, lcd_D6_bit
; set up D5
    sbi     lcd_D5_port, lcd_D5_bit
    sbrs    temp, 5
    cbi     lcd_D5_port, lcd_D5_bit
; set up D4
    sbi     lcd_D4_port, lcd_D4_bit
    sbrs    temp, 4
    cbi     lcd_D4_port, lcd_D4_bit
; write the data
                                            ; 'Address set?up time' (40 nS)
    sbi     lcd_E_port, lcd_E_bit           ; Enable pin high
    call    delay1uS                        ; implement 'Data set?up time' (80 nS) and 'Enable pulse width' (230 nS)
    cbi     lcd_E_port, lcd_E_bit           ; Enable pin low
    call    delay1uS                        ; implement 'Data hold time' (10 nS) and 'Enable cycle time' (500 nS)
    ret
; ============================== End of 4?bit LCD Subroutines ===============
; ============================== Time Delay Subroutines =====================
; Name:     delayYx1mS Delay of (YH:YL) x 1 mS
delayYx1mS:
    call    delay1mS                        ; delay for 1 mS
    sbiw    YH:YL, 1                        ; update the the delay counter
    brne    delayYx1mS                      ; counter is not zero
; arrive here when delay counter is zero (total delay period is finished)
    ret
; ???????????????????????????????????????????????????????????????????????????
; Name:     delayTx1mS Provide a delay of (temp) x 1 mS
delayTx1mS:
    call    delay1mS                        ; delay for 1 mS
    dec     temp                            ; update the delay counter
    brne    delayTx1mS                      ; counter is not zero
; arrive here when delay counter is zero (total delay period is finished)
    ret
; ???????????????????????????????????????????????????????????????????????????
; Name:     delay1mS ?? Delay of 1 mS
delay1mS:
    push    YL                              ; [2] preserve registers
    push    YH                              ; [2]
    ldi     YL, low (((fclk/1000)-18)/4)    ; [1] delay counter
    ldi     YH, high(((fclk/1000)-18)/4)    ; [1]
delay1mS_01:
    sbiw    YH:YL, 1                        ; [2] update the the delay counter
    brne    delay1mS_01                     ; [2] delay counter is not zero
; arrive here when delay counter is zero
    pop     YH                              ; [2] restore registers
    pop     YL                              ; [2]
    ret                                     ; [4]
; ???????????????????????????????????????????????????????????????????????????
; Name:     delayTx1uS Delay of (temp) x 1 uS with a 8 MHz clock frequency
delayTx1uS:
    call    delay1uS                        ; delay for 1 uS
    dec     temp                            ; decrement the delay counter
    brne    delayTx1uS                      ; counter is not zero
; arrive here when delay counter is zero (total delay period is finished)
    ret
; ???????????????????????????????????????????????????????????????????????????
; Name:     delay1uS
; Purpose:  Delay of 1 uS with a 8 MHz clock frequency
delay1uS:
    push    temp                            ; [2] Consume clock cycles
    pop     temp                            ; [2]
    ret                                     ; [4]
; ============================== End of Time Delay Subroutines =============

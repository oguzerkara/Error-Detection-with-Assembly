;.include "m128def.inc"
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
.equ    lcd_E_port          = PORTC         ; lcd Enable pin
.equ    lcd_E_bit           = PORTC0
.equ    lcd_E_ddr           = DDRC
.equ    lcd_RS_port         = PORTC         ; lcd Register Select pin
.equ    lcd_RS_bit          = PORTC2
.equ    lcd_RS_ddr          = DDRC
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

;=======================ADDITIONALS============================

.def	num1				= R17
.def	num2				= R18
.def	nummode				= R19
.def	counter				= R20
.equ	ONES				= 0xFF
.equ	ZEROS				= 0x00

.equ	result1				= 0x0100
.equ	result2				= 0x0200



; jump over Interrupt Vectors, Program ID etc.
; ****************************** Main Program Code **************************
.org    0x0000
; initialize the stack pointer to the highest RAM address
    ldi     temp,low(RAMEND)
    out     SPL,temp
    ldi     temp,high(RAMEND)
    out     SPH,temp
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/  MY TURN  \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;	call	cln_result1
;	call	cln_result2
;	call	disp_result
MAIN:
	call	get_input
	call	code
	call	LCD_init

rjmp	MAIN

.org	0x005000
code:
	ldi		R24, 0x30
	ldi		R25, 0x31

	MOV		temp, nummode
	subi	temp, 0
	breq	modezero

	MOV		temp, nummode
	subi	temp, 1
	breq	modeone

	MOV		temp, nummode
	subi	temp, 2
	breq	modetwo

	MOV		temp, nummode
	subi	temp, 3
	breq	modethree
	ret

modezero:
	ldi		counter, 7
	mov		R26, num1
	ROL		R26
	call	rep
	inc		R23
	call	parity
	call	add_null
	ret
modeone:
	ldi		counter, 7
	mov		R26, num1
	ROL		R26
	call	rep
	call	parity
	call	add_null
	ret
modetwo:
	call	subst
	breq	eq
	sbrc	R21, 7
	mov		R26, num2
	sbrs	R21, 7
	mov		R26, num1
	call	conv_to_hex
	ret
modethree:
	ldi		counter, 8
	call	subst
	breq	eq
	sbrc	R21, 7
	neg		R21
	mov		R26, R21
	call	rep
	mov		R26, R21
	call	conv_to_hex
	ret
subst:
	MOV		R21, num1
	MOV		temp, num2
	SUB		R21, temp
	ret
eq:
	ldi		ZH,	HIGH(result1)
	ldi		ZL,	LOW(result1)
	ldi		R23, 'E'
	st		Z+,	 R23
	ldi		R23, 'Q'
	st		Z+,	R23
	ret

;___________________________________________________________________________________________BIN-HEX CONVERTION SUBROUTINE___________________________________________________________________________________________
conv_to_hex:
	ldi		ZH,	HIGH(result1)
	ldi		ZL,	LOW(result1)
	mov		temp, R26
	swap	temp
	andi	temp, 0x0F
	add		temp, R24
	st		Z+, temp
	mov		temp, R26
	andi	temp, 0x0F
	sbrc	temp, 3
	call	overeight
	add		temp, R24
	st		Z+, temp
	ldi		R23, ZEROS
	st		Z,	R23
	ret
overeight:
	subi	temp, 0x0A
	brmi	underten
	ldi		R23, 0x41
	add		temp, R23
	st		Z+, temp
	ldi		R23, ZEROS
	st		Z,	R23
	ret
underten:
	mov		temp, R26
	andi	temp, 0x0F
	add		temp, R24
	st		Z+, temp
	ldi		R23, ZEROS
	st		Z,	R23
	ret
;___________________________________________________________________________________________ODD PARITY SUBROUTINE___________________________________________________________________________________________
rep:
	ldi		ZH,	HIGH(result2)
	ldi		ZL,	LOW(result2)
	ldi		R23,	ZEROS
eminem:
	sbrc	R26,7
	inc		R23
	sbrc	R26,7
	st		Z+,	R25
	sbrs	R26, 7
	st		Z+,	R24
	ROL		R26
	dec		counter
	brne	eminem
	ret
parity:
	sbrc	R23, 0
	ldi		R20, 0x31
	sbrs	R23, 0
	ldi		R20, 0x30
	st		Z+,	R20
	ret
add_null:
	ldi		R23, ZEROS
	st		Z, R23
	ret


rjmp	code

;___________________________________________________________________________________________INPUT SUBROUTINE___________________________________________________________________________________________

.org	0x007000
get_input:
	ldi		temp,	ZEROS
	out		DDRD,	temp
	in		nummode, PIND
	out		DDRA,	temp
	in		num1,	PINA
	out		DDRB,	temp
	in		num2,	PINB
	cpi		num1,	ZEROS
	brmi	get_input
	subi	num2,	ZEROS
	brmi	get_input
	ret

;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/  MY TURN END  \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
.org	0x009000
LCD_init:
; configure the microprocessor pins for the data lines
    sbi     lcd_D7_ddr, lcd_D7_bit          ; 4 data lines ? output
    sbi     lcd_D6_ddr, lcd_D6_bit
    sbi     lcd_D5_ddr, lcd_D5_bit
    sbi     lcd_D4_ddr, lcd_D4_bit
; configure the microprocessor pins for the control lines
    sbi     lcd_E_ddr,  lcd_E_bit           ; E line ? output
    sbi     lcd_RS_ddr, lcd_RS_bit          ; RS line ? output
; initialize the LCD controller
    call    lcd_init_4d                     ; initialize the LCD display for a 4-bit interface

	;___________________________________________________________________________________________DISPLAY RESULTS___________________________________________________________________________________________

	ldi		ZH,	HIGH(result1)
	ldi		ZL,	LOW(result1)
	ldi		temp, lcd_LineOne
	call	lcd_write_string_4d

	ldi		ZH,	HIGH(result2)
	ldi		ZL,	LOW(result2)
	ldi		temp, lcd_LineTwo
	call	lcd_write_string_4d

	ret

	;___________________________________________________________________________________________CLEAN THE LCD SUBROUTINE___________________________________________________________________________________________

; ****************************** End of Main Program Code *******************
; ============================== 4?bit LCD Function Calls ======================

;-----------------------------------------START OF INITIALIZATION SUBROUTINE-----------------------------------------
;-----------------------------------------START OF INITIALIZATION SUBROUTINE-----------------------------------------

lcd_init_4d:
; Power?up delay
    ldi     temp, 100                       ; initial 40 mSec delay
    call    delayTx1mS
; Set up the RS and E lines for the 'lcd_write_4' subroutine.
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
; Preliminary Function Set instruction ? used only to set the 4?bit mode.
; The number of lines or the font cannot be set at this time since the controller is still in the
;   8?bit mode, but the data transfer mode can be changed since this parameter is determined by one
;   of the upper four bits of the instruction.
    ldi     temp, lcd_FunctionSet4bit       ; set 4?bit mode
    call    lcd_write_4
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
; Function Set instruction
    ldi     temp, lcd_FunctionSet4bit       ; set mode, lines, and font
    call    lcd_write_instruction_4d
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
; The next three instructions are specified in the data sheet as part of the initialization routine,
;   so it is a good idea (but probably not necessary) to do them just as specified and then redo them
;   later if the application requires a different configuration.
; Display On/Off Control instruction
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
; This is the end of the LCD controller initialization as specified in the data sheet, but the display
;   has been left in the OFF condition.  This is a good time to turn the display back ON.
; Display On/Off Control instruction
    ldi     temp, lcd_DisplayOn             ; turn the display ON
    call    lcd_write_instruction_4d
    ldi     temp, 80                        ; 40 uS delay (min)
    call    delayTx1uS
    ret
;---------------------------------------------END OF INITIALIZATION SUBROUTINE-----------------------------------------
;---------------------------------------------END OF INITIALIZATION SUBROUTINE-----------------------------------------





;########################################################################################################################
;-------------------------------MOST USED WRITE SUBRUTINES-----------------------------------------------------------
;########################################################################################################################


; ???????????????????????????????????????????????????????????????????????????????????????????????????????????????
;     ******************************* START OF CHAR AND STRINGS *********************************************
; ???????????????????????????????????????????????????????????????????????????????????????????????????????????????

; Name:     lcd_write_string_4d  display a string of characters on the LCD
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
    ld		temp, Z                       ; get a character
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
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

; Name:     lcd_write_character_4d
lcd_write_character_4d:
    sbi     lcd_RS_port, lcd_RS_bit         ; select the Data Register (RS high)
    cbi     lcd_E_port, lcd_E_bit           ; make sure E is initially low
    call    lcd_write_4                     ; write the upper 4?bits of the data
    swap    temp                            ; swap high and low nibbles
    call    lcd_write_4                     ; write the lower 4?bits of the data
    ret


; ???????????????????????????????????????????????????????????????????????????????????????????????????????????????
;           *******************************END OF CHAR AND STRINGS*********************************************
; ???????????????????????????????????????????????????????????????????????????????????????????????????????????????

; Name:     lcd_write_instruction_4d ?? Send a byte of information to the LCD instruction register
lcd_write_instruction_4d:
    cbi     lcd_RS_port, lcd_RS_bit         ; select the Instruction Register (RS low)
    cbi     lcd_E_port, lcd_E_bit           ; make sure E is initially low
    call    lcd_write_4                     ; write the upper 4?bits of the instruction
    swap    temp                            ; swap high and low nibbles
    call    lcd_write_4                     ; write the lower 4?bits of the instruction
    ret
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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


;########################################################################################################################
;-------------------------------END OF MOST USED WRITE SUBRUTINES----------------------------------------------------
;########################################################################################################################
; ============================== End of 4?bit LCD Subroutines ===============



; ============================== Time Delay Subroutines =====================

delayTxS:
	LP_tmp:	ldi		R20, 10
	LP_1:	ldi		R21, 50
	LP_5:	call	delayTx1mS
			dec		R21
			brne	LP_5
			dec R20
			brne LP_1
			dec temp
			brne LP_tmp
	ret

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
    push    YL
	push    YH
                           ; [2] preserve registers
    ldi     YL, low (1996)
	ldi     YH, high(1996)

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
; ============================== End of Time Delay Subroutines ==============

;
; AVR-Assembly Embedded Programming Project:
; This project was designed to simulate the affects of measuring temperature
; and humidity on a microcontroller by using a random number generator for the
; temp/humidity values and then displaying them on an LCD screen along with
; my name and UVIC student number.
;
; Created: 4/7/2019 1:59:05 PM
; Author : nolancaissie
;

#define LCD_LIBONLY

.equ TOP = 31250
.equ A = 71
.equ C = 17
.equ SEED = 19
.equ ADC_BTN_VAL = 0x316
.def AdcBtnValLow = r24	
.def AdcBtnValHigh = r25
.def DisplayMode = r23
.def Char0 = r22
.def CharSpace = r21

.cseg
.org 0

.org 0x0000
	rjmp reset

.org 0x0024
	rjmp isr_timer1_cmpb

.org 0x003A
	rjmp isr_adc_c

.org 0x0074

.include "lcd.asm"

.cseg
reset:

	;init Timer/Counter 1
	ldi r16, 0x20                       ; clear register OC1A on Compare Match
	sts TCCR1A, r16 
	ldi r16, 0x05                       ; timer clock = system clock/64
	sts TCCR1B, r16
	ldi r16, 0b00000000
	sts TCCR1C, r16 
	ldi r16, 0x04                       ;Output Compare A Interrupt
	sts TIMSK1, r16
	ldi r16, high(TOP)                  ;TOP Value
	sts OCR1BH, r16
	ldi r16, low(TOP)
	sts OCR1BL, r16
	
	;init ADC
	ldi r16, 0x40	; out AVcc
	sts ADMUX, r16
	ldi r16, 0x05	; use Timer 1 Compare Match B to trigger ADC conversion	
	sts ADCSRB, r16
	ldi r16, 0xaf	; enable ADC, auto-trigger and ADC complete interrupt. with 128 prescaler
	sts ADCSRA, r16
	ldi r16, 0xfe	; disable other ADC pins except ADC 0
	sts DIDR0, r16
	ser r16
	sts DIDR2, r16

	clr r16 ;clear

	; other initializations
	ldi AdcBtnValLow, low(ADC_BTN_VAL)
	ldi AdcBtnValHigh, high(ADC_BTN_VAL)
	;lds r16, ADCSRA ; start the first ADC conversion by set ADSC=1
	;ri r16, 0x40   ; 0x40 == 1<<ADSC
	;sts ADCSRA, r16
	ldi r17, 0
	ldi r20, 0

	
	
	call lcd_init			; call lcd_init to Initialize the LCD (line 689 in lcd.asm)
	; copy string templates from program memory to data memory
	; i created a function string_to_data that will pass the parameters by reference
	ldi XL, low(vstr)
	ldi XH, high(vstr)
	ldi ZL, low(init_v<<1)
	ldi ZH, high(init_v<<1)
	rcall string_to_data
	; show v-number
	; Now move the cursor to the second line (ie. 0,1)
	push r16
	call lcd_clr
	ldi r16, 0x01
	push r16
	ldi r16, 0x0A
	push r16
	call lcd_gotoxy
	pop r16
	pop r16

	; Now display vnum on the second line
	ldi r16, high(vstr)
	push r16
	ldi r16, low(vstr)
	push r16
	call lcd_puts
	pop r16
	pop r16

	pop r16
	
	ldi ZL, low(init_h<<1)
	ldi ZH, high(init_h<<1)
	ldi XL, low(hstr)
	ldi XH, high(hstr)
	rcall string_to_data
	
	ldi ZL, low(init_t<<1)
	ldi ZH, high(init_t<<1)
	ldi XL, low(tstr)
	ldi XH, high(tstr)
	rcall string_to_data

	

lp:	jmp lp
				
	sei

done:
	rjmp done

isr_adc_c:

	;check button pressing, change display mode accordingly
	;check button pressing, change display mode accordingly
	lds XL, ADCL
	lds XH,	ADCH
	
	cp XL, AdcBtnValLow
	cpc XH, AdcBtnValHigh
	brlo btn_press
	clr r17; not pressed

	btn_press:
		inc r17
		cpi r17, 2
		brlt do_stuff
		cpi r20, 1
		breq set_humidity
		ldi r20, 1
		rjmp do_stuff
		set_humidity:
		ldi r20, 0
		do_stuff:
			cpi r20, 0
			breq humidity
			temperature:;temperature, signed bit
	; Now move the cursor to the second line (ie. 0,1)
	push r16
	ldi r16, 0x00
	push r16
	ldi r16, 0x00
	push r16
	call lcd_gotoxy
	pop r16
	pop r16
	; Now display temp on the first line
	ldi r16, high(tstr)
	push r16
	ldi r16, low(tstr)
	push r16
	call lcd_puts
	pop r16
	pop r16

	pop r16

		rcall rand ;generate random value
		;rcall itoa ;convert to signed mag string
		rcall led ;illuminate LEDs
		;rcall itoa_binary ;show binary string
		sbrs r16, 5
		rjmp pos
		if_neg:
		rcall itoa ;takes a value and turn it into a signed magnitude string
		;rcall show_binary_str ;show binary string
		rcall show_decimal_str_neg ;show decimal string
		rjmp del
		pos:
		rcall itoa ;takes a value and turn it into a signed magnitude string
		;rcall show_binary_str ;show binary string
		rcall show_decimal_str ;show decimal string
		del:
		rcall delay ;delay 1 second
		;clr r17
		;clr r20

	rjmp after


			
			humidity:;humidity, unsigned bit
	push r16
	ldi r16, 0x00
	push r16
	ldi r16, 0x00
	push r16
	call lcd_gotoxy
	pop r16
	pop r16

	; Now display humidity on the first line
	ldi r16, high(hstr)
	push r16
	ldi r16, low(hstr)
	push r16
	call lcd_puts
	pop r16
	pop r16

	pop r16

		rcall rand ;generate random value
		rcall led ;illuminate LEDs
		;rcall itoa_binary ;show binary string
		rcall itoa_decimal ;turn value into unsigned string
		;rcall show_binary_str ;show binary string
		rcall show_decimal_str ;show decimal string
		rcall delay ;delay 1 second
		;clr r17
		;clr r20


	after:
	reti
			

	reti
isr_timer1_cmpb:
	reti




;this function will move the vnum and temp and humidity strings to data memory
string_to_data:	
loop:
	lpm r16, Z+
	st X+, r16
	cpi r16, '0'
	brne loop
	ret 









;ASSIGNMENT 3 QUESTION 5
.cseg
;initialization (comment because it conflicts with LCD driver,
;by turning off the background light of LCD)
;ldi r16, 0b00001010
;out DDRB, r16
;ldi r16, 0b10101010
;sts DDRL, r16h
rcall lcd_init
ldi r16, SEED


;description: ; generate a random number in range 0~63
;input: R16 - seed or previous random number
;output:R16 - new random number
rand:
	push r17
	push r1
	push r0
	ldi r17, A
	mul r16, r17
	mov r16, r0
	ldi r17, C
	add r16, r17
	andi r16, 0b00111111
	pop r0
	pop r1
	pop r17
	ret
;description: ; illuminate LEDs
;input: R16 - controlling value
;output:none
led:
	push r17
	clr r17
	sbrc r16, 0
	ori r17, 0b00000010
	sbrc r16, 1
	ori r17, 0b00001000
	out PORTB, r17
	clr r17
	sbrc r16, 2
	ori r17, 0b00000010
	sbrc r16, 3
	ori r17, 0b00001000
	sbrc r16, 4
	ori r17, 0b00100000
	sbrc r16, 5
	ori r17, 0b10000000
	sts PORTL, r17
	pop r17
	ret

;description: convert a value to decimal string
;input: R16 - the value
;output: "dstr" in data memory
itoa_decimal:
	push r16
	push r17
	rcall div_by_10
	ldi r17, '0'
	add r0, r17
	add r1, r17
	ldi r16, ' '
	cpse r1, r17
	mov r16, r1
	sts dstr, r16
	sts dstr+1, r0
	ldi r16, '\0'
	sts dstr+2, r16
	pop r17
	pop r16
	ret

;description: show decimal on LCD at the 1st row 
;input: string in data memory with label "dstr"
;output: none
show_decimal_str:
	push r16
	ldi r16, 0x00
	push r16
	ldi r16, 0x03
	push r16
	call lcd_gotoxy
	pop r16
	pop r16
	ldi r16, high(dstr)
	push r16
	ldi r16, low(dstr)
	push r16
	call lcd_puts
	pop r16
	pop r16
	pop r16
	ret

;description: show decimal on LCD at the 1st row 
;input: string in data memory with label "dstr"
;output: none
show_decimal_str_neg:
	push r16
	ldi r16, 0x00
	push r16
	ldi r16, 0x02
	push r16
	call lcd_gotoxy
	pop r16
	pop r16
	ldi r16, high(dstr)
	push r16
	ldi r16, low(dstr)
	push r16
	call lcd_puts
	pop r16
	pop r16
	pop r16
	ret
;description: delay for some time
;input: none
;output: none
delay:
	push r16
	ldi r16, 0
	loop_delay:
		call dly_ms
		inc r16
		cpi r16,250

	brlo loop_delay
	pop r16
	ret
;description: divide by 10
;input: R16 - dividend
;output: R1 - quotient, R0 - remainder
div_by_10:
	push r16
	clr r1
db10_loop:
	cpi r16, 10
	brlo db10_done
	subi r16, 10
	inc r1
	rjmp db10_loop
db10_done:
	mov r0, r16
	pop r16
	ret

;description: convert a signed magnitude value into 0-ending string
;input: R16 - the value
; X - starting address of the string
;output:none
itoa:
	ldi XL, low(dstr) ;need to initialize space in memory for the string
	ldi XH, high(dstr)
	push r16
	push r17
	push r2
	push r1
	push r0
	sbrs r16, 5 ;if bit 5 is a one aka the number is negative it will place a negative in the string else it will just convert to string
	rjmp next
	ldi r17, '-'
	st X+, r17

next:
	andi r16, 0b01111111
	ldi r17, '0'
	rcall div_by_10
	add r0, r17
	mov r2, r0
	mov r16, r1
	rcall div_by_10
	add r0, r17
	add r1, r17
	cp r1, r17
	cpse r1, r17
	st X+, r1 ;only store when has hundreds
	add r1, r0
	add r17, r17
	cpse r1, r17 ;only store when has tens or hundreds
	st X+, r0
	st X+, r2
	clr r17 ; add a 0-ending
	st X+, r17
	pop r0
	pop r1
	pop r2
	pop r17
	pop r16
	ret
;description: divide by 10
;input: R16 - dividend
;output: R1 - quotient, R0 - remainder
loop_db10:
	cpi r16, 10
	brlo done_db10
	subi r16, 10
	inc r1
	rjmp loop_db10
done_db10:
	mov r0, r16
	pop r16

	ret

	

	init_v: .db "V-8989", 0, 0
	init_h: .db "H:   %", 0, 0
	init_t: .db "T:   C", 0, 0

.dseg
	hstr: .byte 20
	tstr: .byte 20
	vstr: .byte 20
.dseg
dstr: .byte 100 ;for decimal display (signed and unsigned)

;everything is working but i need to make the signed string show for temp
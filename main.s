
       THUMB
       AREA    DATA, ALIGN=2
       ALIGN          
       AREA    |.text|, CODE, READONLY, ALIGN=2
       EXPORT  Start
; Masked addr for each switch on GPIO_F
RESETB EQU 0x40025004 ; PF0
GO EQU 0x40025020 ; PF3
STOP EQU 0x40025040 ; PF4
; Base Addresses and Unlock codes/address
RCGCGPIO EQU 0x400FE108 ; Clocks for GPIOs
UNLOCK EQU 0x4C4F434B ; Unlock code for GPIO Port F
PORTF EQU 0x40025000 ; GPIO Port F
PORTC EQU 0x40006000 ; GPIO Port C
PORTB EQU 0x40005000 ; GPIO Port B
PORTE EQU 0x40024000 ; GPIO Port E
STCTRL EQU 0xE000E010  ; STCTRL
STRELOAD EQU 0xE000E014  ; STRELOAD 
STCURRENT EQU 0xE000E018  ; STCURRENT 


Start
	BL GPIOINIT
	BL SYSTICK_INIT
Init
    BL INSERTCOIN
; Reset

;--------------------------------
; Game Start
;--------------------------------


	B   Init

;; Subroutine to unlock and initialize GPIO Ports
GPIOINIT
	PUSH {LR}
	;------------------------------------------------------
	; Initialize GPIO Ports
	LDR R0, =RCGCGPIO ; Enable GPIO Clock RCGCGPIO
	MOV R1, #0x36 ; Enables GPIO Port B, C, E, and F
	STR R1, [R0] ;
    NOP
    NOP

	;-----------------------
	; Unlock Port F
	LDR R0, =PORTF
	LDR R1, =UNLOCK
	STR R1, [R0,#0x520];
	; Enable port F for switches
	MOV R1, #0x19 ; enable bits 0,3,4 for switches
	STR R1, [R0,#0x524]; GPIOCR enable [0,3,4]
	STR R1, [R0,#0x510]; GPIOPUR on [0,3,4]
	MOV R1, #0xE6 ; [0,3,4] are inputs
	STR R1, [R0,#0x400]; GPIODIR set as above
	MOV R1, #0x19
	STR R1, [R0,#0x51C] ;GPIODEN [0,3,4]
	
	; Enable port C for lower 4 bits of LED
	LDR R0, =PORTC
	LDR R1, =UNLOCK
	STR R1, [R0,#0x520];

	MOV R1, #0xF0;
	STR R1, [R0,#0x524]; GPIOCR enable [7:4]
	MOV R1, #0xF0; ; [7:4] are outputs
	STR R1, [R0,#0x510] ; GPIOPUR [7:4]
	STR R1, [R0,#0x400] ; GPIODIR set as above
	STR R1, [R0,#0x51C] ; GPIODEN [7:4]
    STR R1, [R0,#0x3C0]  ; Initialize LEDs to OFF


	; Enable port E for higher 6 bits of LED
	LDR R0, =PORTE
	MOV R1, #0x3F;
	STR R1, [R0,#0x524] ; GPIOCR enable [5:0]
	MOV R1, #0x3F; ; [5:0] are outputs
	STR R1, [R0,#0x400] ; GPIODIR set as above
	STR R1, [R0,#0x510] ; GPIOPUR [5:0]
	STR R1, [R0,#0x51C] ; digital enable bits[5:0]
    STR R1, [R0,#0xFC]  ; Initialize LEDs to OFF
  
	; Enable port B for DIP switch
	LDR R0, =PORTB
	MOV R1, #0xF;
	STR R1, [R0,#0x524] ; GPIOCR enable [3:0]
	MOV R1, #0x0       ; [3:0] are inputs
	STR R1, [R0,#0x400] ; GPIODIR set as above
	STR R1, [R0,#0x510] ; GPIOPUR [3:0]
	STR R1, [R0,#0x51C] ; digital enable bits[3:0]
	POP {LR}
	BX LR

;----------------------
; Initialize SysTick
; Modifies: R0, R1
;----------------------
SYSTICK_INIT
	PUSH {LR}
	LDR R1, =STCTRL
	MOV R0, #0
	STR R0, [R1]       ; Disable SysTick during initialization
	LDR R1, =STRELOAD   ; initialize to max value
	LDR R0, =0xFFFFFF ; 
	STR R0, [R1]
	LDR R1, =STCURRENT
	MOV R0, #0
	STR R0, [R1]
	LDR R1, =STCTRL
	MOV R0, #5         ; Use internal clock and start counting
	STR R0, [R1]
	POP {LR}
	BX LR


;----------------------
	; Print Subroutine
	;  takes a parameter R1 = value to display
	;  Preserves R4 - R11 by AAPCS
	;  returns nothing
	; Displays R1[9:0] on LED bar
	; PORTC[7:4] = Counter[3:0]
	; PORTE[5:0] = Counter[9:5]
	; Uses temp R5 register to invert bits (active low)
	; Also shifts around R5 into temp R10 register to position bits
	; into proper bit position for ports.
Print
	PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR} ; Nice Stack Frame usage
	MVN R5, R1; ; Invert counter due to active low LEDs
	LDR R0, =PORTC
	LSL R10, R5, #4 ; get lower bits [3:0] of counter into bit position [7:4] of R10
	STR R10, [R0,#0x3C0] ; write counter[3:0] into Port C[7:4]
	LDR R0, =PORTE
	LSR R10, R5, #4 ; rotate counter upper 6 bits for LED display into position [5:0]
	STR R10, [R0,#0x0FC] ; write upper six bits into LED disply
	POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	BX LR

;----------------------------------------------
; INSERTCOIN Subroutine
;  returns nothing
;  Start of game, waiting for P1 and P2 Start button pushes
INSERTCOIN
;   R4 = OFF_State of LEDs
;   R5 = ON_State of LEDs

; Define initial OFF / ON patterns
    MOV R5, #0x30 ; Initial LED ON_pattern defined
                  ;  0000110000
    MOV R2, #0    ; Initial LED OFF_pattern defined
                  ;  0000000000

flashloop
    ; Print OFF Pattern
    MOV R1, R2;
    BL Print

;; Input: R3: 0 == off cycle, 1== on cycle
;; INPUT: R2: OFF_MASK
;; OUTPUT: R2: OFF_MASK (possibly updated)
;; OUTPUT R1: [00], [01] P2 pushed, [10] P1 pushed, [11] tied
    MOV R3, #0
	BL Wait025s
       ; R2 has been modified if necessary by the function as the updated off-cycle mask

    ; Print ON Pattern
    MOV R1, R5;       move LED On pattern to print
	BL Print

    MOV R3, #1
    BL Wait025s

	B    flashloop

;; --------------
;; Returns R1:  [00], [01] P2 pushed, [10] P1 pushed, [11] tied
Wait5ms
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}	
    LDR R0, =0x13880
    SUB R0, #1          ; delay-1 because systick needs 1 clk cycle to set flag
	LDR R1, =STRELOAD
	STR R0, [R1]        ; set RELOAD value and start counting
    LDR R4, =STCTRL
    MOV R6, #0
loop
    BL P1Pushed
    CMP R1, #0
    IT EQ
    MOVEQ R7, #2            ; P1 was pushed
    BL P2Pushed
    CMP R1, #0
    IT EQ
    MOVEQ R6, #1          ; P2 was pushed
    ORR R1, R7, R6;       ; Assemble the return value
                          ; R1[1:0] = 00 (no push) 10 P1 pushed, 01 P2 pushed
    LDR R5, [R4]          ; 
    ANDS R5, R5, #0x10000   ; is count set?
	BEQ loop    
    POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	BX LR

;;-------------------------------
;; Calls Wait5ms 50 times to equal 0.25 seconds
;; Input: R3: 0 == off cycle, 1== on cycle
;; INPUT: R2: OFF_MASK
;; OUTPUT: R2: OFF_MASK (possibly updated)
;; OUTPUT: R1: random number
    ;; already not used ;; OUTPUT R1: [00], [01] P2 pushed, [10] P1 pushed, [11] tied
Wait025s
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}	
DELAY EQU 0x32   ; 50 times Wait5ms = 0.25 sec
    MOV R4, #DELAY
wait025loop
    BL Wait5ms
    CMP R1, #2   ; P1 pushed
    IT EQ
    ORREQ R2, #0x20   ; change OFF mask so P1 ON and ready
    CMP R1, #1   ; P2 pushed
    IT EQ
    ORREQ R2, #0x10


    ; check if off-cycle, if so, move generated off-cycle mask to return
    CMP R3, #0
    ITT EQ
    MOVEQ R1, R2;   ; print modified off-cycle mask immediately
    BLEQ Print      ; Print the OFF_MASK

    ; check if both buttons have been pushed
    CMP R2, #0x30
    LDR R0, =STCURRENT;
    LDR R1, [R0]      ;  R1 becomes a random number = # CC < 5 msec
    BEQ STARTGAME

    SUBS R4, #1
    BEQ exitwait025loop
    B wait025loop
exitwait025loop
    
    POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	BX LR
;
; Reads P2 Button
; Returns R1  ; 0 means P2 Pushed button
P2Pushed
	PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    LDR R4, =PORTF
    LDR R1, [R4, #0x40]   ; R1[4]==0 means P2Pushed Pushed
	POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	BX LR

; Read Port F for P1 push
; Returns R1[3]
; R1[3]==0 means P1 Pushed button
P1Pushed
	PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	LDR R4, =PORTF ;
	LDR R1, [R4, #0x20]   ; R1[3]==0 means P1Pushed Pushed
	POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	BX LR

;; --------------
;; Input: R0 number of times to wait 0.25 seconds before returning
Wait1to2s
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}	
	SUB R0, #1          ; delay-1 because systick needs 1 clk cycle to set flag
	LDR R1, =STRELOAD
	STR R0, [R1]        ; start counting seconds
    LDR R1, =STCURRENT
	STR R0, [R1]        ; start counting seconds
	LDR R1, =STCTRL
to2loop
    LDR R3, [R1]        ; R3 = STCTRL
	ANDS R3, R3, #0x10000   ; is count set?
	BEQ to2loop
 	POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	BX LR


;; 
STARTGAME
; START OF A MOVE
    ; Random Delay > 1sec but less than 2 sec
    AND R1, #3         ; examine only bits [1:0] of random value
    LDR R0, =0x509100      ; 5280000 cycles in 0.33 seconds
    MUL R1, R0        ; R1 becomes random number = 0 seconds < # CC < 1 second
    LDR R2, =0xF42400   ; #CC (16,000,000 * 62.5 ns = 1second)
    ADD R0, R2, R1    ; R0 becomes random 1 to 2 seconds
    BL Wait1to2s

    ; Initialize positions
    MOV R4, #0x20    ; P1 Initial pos
    MOV R5, #0x10    ; P2 Initial pos

    ; Separate LEDS
    LSL R4, #1       ; P1 step back
    LSR R5, #1       ; P2 step back
    ORR R1, R4, R5 ; Present positions
    BL Print

MOVESTART
    ; Read buttons

;---------------------------
; P1PushedFirst
; Start Delay timer  (counter for draws)
; P1Moves
    LSR R4, #1       ; P1 step fwd

    ; P2 MovesInTime
        LSL R5, #1       ; P2 step fwd
        ; Draw, increment draw

; else, P1 Total Win
    ; P1MovesAgain
        LSR R4, #1       ; P1 step fwd
    
    ;  Go to Start of move
    B MOVESTART

;---------------------------
; P2PushedFirst
; Start Delay timer  (counter for draws)
; P2Moves
    LSL R5, #1       ; P2 step fwd

    ; P1 MovesInTime
        LSR R4, #1       ; P1 step fwd
        ; Draw, increment draw

; else, P2 Total Win
    LSL R5, #1       ; P2 step fwd

;  Go to Start of move
    B MOVESTART


    ; Check if GameOver
    ORR R1, R4, R5 ; Present positions
    CMP R1, #0x3
    BEQ GAMEOVER
    CMP R1, #0x300
    BEQ GAMEOVER

; Flash @ 2 Hz
GAMEOVER
    ; print LEDs ON
    ORR R1, R4, R5 ; Present positions
    BL Print

    ; Wait 0.25 seconds
    MOV R10, #DELAY
gameoverloop
    BL Wait5ms
    SUBS R10, #1
    CMP R10, #0
    BNE gameoverloop

    ; print LEDs OFF
    MOV R1, #0;
    BL Print
    
; Wait 0.25 seconds
    MOV R10, #DELAY
gameoverloop2
    BL Wait5ms
    SUBS R10, #1
    CMP R10, #0
    BNE gameoverloop2
    
    B GAMEOVER

       ALIGN      
       END  
           

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

;; Added a comment

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
	MOV R1, #0x0;
	STR R1, [R0,#0x420] ; disable alternative function
	STR R1, [R0,#0x400] ; GPIODIR set as above
	MOV R1, #0xF;
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

	LDR R1, =STCTRL
	MOV R0, #0
	STR R0, [R1]       ; Disable SysTick during initialization
	LDR R1, =STRELOAD   ; initialize to max value
	LDR R0, =0x13880 ; 
	STR R0, [R1]
	LDR R1, =STCURRENT
	MOV R0, #0
	STR R0, [R1]
	LDR R1, =STCTRL
	MOV R0, #5         ; Use internal clock and start counting
	STR R0, [R1]



;LDR R0, =0x13880
;    SUB R0, #1          ; delay-1 because systick needs 1 clk cycle to set flag
;	LDR R1, =STRELOAD
;	STR R0, [R1]        ; set RELOAD value and start counting
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
;; Input: R0 number of clock cycles to wait equivalent to 1 to 2 seconds before returning
Wait1to2s
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}	
	SUB R0, #1          ; delay-1 because systick needs 1 clk cycle to set flag
	LDR R1, =STRELOAD
	STR R0, [R1]        ; start counting seconds
    LDR R1, =STCURRENT
	MOV R0, #5
	STR R0, [R1]        ; start counting seconds
	LDR R1, =STCTRL
to2loop
    LDR R3, [R1]        ; R3 = STCTRL
	ANDS R3, R3, #0x10000   ; is count set?
	BEQ to2loop
 	POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	BX LR


STARTGAME
    ; Initialize positions
    MOV R4, #0x20    ; P1 Initial pos
    MOV R5, #0x10    ; P2 Initial pos
	MOV R8, #0       ; Contiguous drawn move counter
 
STARTMOVE
; START OF A MOVE
    ; Random Delay > 1sec but less than 2 sec
	LDR R0, =STCURRENT;
    LDR R1, [R0]      ;  R1 becomes a random number = # CC < 5 msec
    AND R1, #3         ; examine only bits [1:0] of random value
    LDR R0, =0x509100      ; 5280000 cycles in 0.33 seconds
    MUL R1, R0        ; R1 becomes random number = 0 seconds < # CC < 1 second
    LDR R2, =0xF42400   ; #CC (16,000,000 * 62.5 ns = 1second)
    ADD R0, R2, R1    ; R0 becomes random 1 to 2 seconds
    BL Wait1to2s

    ; Separate LEDS
    LSL R4, #1       ; P1 step back  R4 is P1 position
    LSR R5, #1       ; P2 step back  R5 is P2 position
    ORR R1, R4, R5 ; Present positions
    BL Print


    ; Read buttons
READButtons	LDR R6, =PORTF
    LDR R1, [R6, #0x60]   ; R1[4]==0 means P2Pushed Pushed
	; 0x00 if no one has pushed (branch to read buttons)
	CMP R1, #0x18
	BEQ READButtons

;;0x10
; 0x10 PF3 was pushed P1
; 0x08 PF4 was pushed P2

	;0x20 if P2 pushed
	CMP R1, #0x08
	BEQ P2PushedFirst
	
	;0x40 if P1 pushed
	CMP R1, #0x10
	BEQ P1PushedFirst

	; 0x60 if both simulatenous (Mario always wins)
	CMP R1, #0x0
	BEQ P1PushedFirst


;---------------------------
; P1PushedFirst
P1PushedFirst

; P1Moves
    LSR R4, #1       ; P1 step fwd
	ORR R1, R4, R5 ; Present positions
    BL Print

; Start Delay timer  (counter for draws)
; Read DIP switches  
; PB[3:0] are dip switches
; PB[3:2] switches for P1
     ; PB[1:0] switches for P2

; Timer equation is 2^-min(d, 4) * (320 - 80*Sn) in ms
; we have a wait5ms subtroutine, we will reuse it by calculating how many times 5ms needs to be waited
; this means timer equations turns to 
; 2^-min(d, 4) * (64 - 16*Sn) times to call wait5ms
	LDR R6, =PORTB
	; Read switches for P1 if P1 Pushed first
    LDR R7, [R6, #0x30]   ; R7 now has P1 switches [3:2]
	LSR R7, #2;
	MOV R2, #16           ; 
	MUL R7, R7, R2
	MOV R2, #64
	SUB R7, R2, R7        ; R7 is running total 

	; compare draw counter and 4, call Calculate exponent f(x)
	CMP R8, #4         
	ITE LS
	MOVLS R2, R8    ; R8 < 4, so we use R8
	MOVHI R2, #4    ; R8 > 4, so we use #4 as exponent parameter
	BL CalcExp
	
	;MOV R0, R7;  why am i moving the return calculated value  FINISH ME
	
	BL WaitP1Delay
	CMP R0, #1     ; 
	BNE P1TotalWin
	; P2 MovesInTime
    LSL R5, #1       ; P2 step fwd
    ORR R1, R4, R5 ; Present positions
    BL Print
 	; Draw, increment draw
	ADD R8, #1
	B STARTMOVE
P1TotalWin
	LSR R4, #1       ; P1 step fwd
	ORR R1, R4, R5 ; Present positions
    BL Print
	MOV R8, #0       ; Clear draw counter
	BL CheckGAMEOVER
	
; WaitP1Delay function, calls wait5ms a certain number of times
; this is called when P1 has pushed first
; it checks if P2 has pushed
; if P2 has pushed before time expires, it returns
; Input: R0, number of times to call wait5ms
; Output: R0, 1 means P2 pushed a button before timeout, 0 otherwise

WaitP1Delay
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}	
    MOV R4, R0   ; loop counter
	MOV R0, #0   ; default return P2 did not push in time
waitdelayloop
    BL Wait5ms
	MOV R5, #0x3
	AND R1, R5
    CMP R1, #1   ; P2 pushed  
    BEQ exitwaitdelayloop
    SUBS R4, #1
    BEQ exitwaitdelaylooptimeout
    B waitdelayloop
exitwaitdelayloop
	MOV R0, #1   ; P2 pushed in time
exitwaitdelaylooptimeout
	POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	BX LR

; Function to calculate exponent
; Input: R2, the exponenet of 2^exp to calculate
; Output: R0, the result
CalcExp
	PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR} ; Nice Stack Frame usage
	MOV R1, #0   ; Loop counter
	MOV R4, #2   ; number to multiply
	MOV R0, #1   ; initialize result to 1
calcexploop
	CMP R1, R2   ; i <= exp?
	BEQ exitcalcexp
	ADD R1, #1
	; Body of code
	MUL R0, R0, R4  ;
	b calcexploop
exitcalcexp
	; R0 has result
	POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	BX LR


;---------------------------
; P2PushedFirst
P2PushedFirst	

	; P2Moves
	LSL R5, #1       ; P2 step fwd
	ORR R1, R4, R5 ; Present positions
    BL Print
	
	LDR R6, =PORTB
	; Read switches for P1 if P1 Pushed first
	;      ; PB[1:0] switches for P2
    LDR R7, [R6, #0xC]   ; R7 now has P2 switches
	MOV R2, #16           ; 
	MUL R7, R7, R2
	MOV R2, #64
	SUB R7, R2, R7        ; R7 is running total 

	; compare draw counter and 4, call Calculate exponent f(x)
	CMP R8, #4         
	IT LS
	MOVLS R1, R8    ; R8 < 4, so we use R8
	MOVHI R1, #4    ; R8 > 4, so we use #4 as exponent parameter
	BL CalcExp
	
	MOV R0, R7;
	
	BL WaitP2Delay
	CMP R0, #1     ; 
	BNE P2TotalWin
	; P1 MovesInTime
    LSR R4, #1       ; P1 step fwd
    ORR R1, R4, R5 ; Present positions
    BL Print
 	; Draw, increment draw
	ADD R8, #1
	B STARTMOVE
P2TotalWin
	LSL R5, #1       ; P2 step fwd
	ORR R1, R4, R5 ; Present positions
    BL Print
	MOV R8, #0       ; Clear draw counter
	BL CheckGAMEOVER

; WaitP2Delay function, calls wait5ms a certain number of times
; this is called when P2 has pushed first
; it checks if P1 has pushed
; if P1 has pushed before time expires, it returns
; Input: R0, number of times to call wait5ms
; Output: R0, 1 means P1 pushed a button before timeout, 0 otherwise

WaitP2Delay
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}	
    MOV R4, R0   ; loop counter
	MOV R0, #0   ; default return P1 did not push in time
waitP2delayloop
    BL Wait5ms
	MOV R5, #0x3
	AND R1, R5
    CMP R1, #0x2   ; P1 pushed  
    BEQ exitwaitP2delayloop
    SUBS R4, #1
    BEQ exitwaitP2delaylooptimeout
    B waitP2delayloop
exitwaitP2delayloop
	MOV R0, #1   ; P1 pushed in time
exitwaitP2delaylooptimeout
	POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
	BX LR

CheckGAMEOVER
    ; Check if GameOver
    ORR R1, R4, R5 ; Present positions
    CMP R1, #0x3
    BEQ GAMEOVER
    CMP R1, #0x300
    BEQ GAMEOVER
	; Not game over, so go back to random timer for next move
	BL STARTMOVE

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
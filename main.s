
       THUMB
       AREA    DATA, ALIGN=2
       ALIGN          
       AREA    |.text|, CODE, READONLY, ALIGN=2
       EXPORT  Start
           
; Bitmasked addr for each button on GPIO_F
RESETBUT  EQU 0x40025004  ; PF0 Not used in code (reference)
P1BUTTON  EQU 0x40025020  ; PF3
P2BUTTON  EQU 0x40025040  ; PF4
    
; Base Addresses and Unlock codes/address
RCGCGPIO  EQU 0x400FE108  ; Clocks for GPIOs
UNLOCK    EQU 0x4C4F434B  ; Unlock code for GPIO Port F
PORTB     EQU 0x40005000  ; GPIO Port B
PORTC     EQU 0x40006000  ; GPIO Port C
PORTE     EQU 0x40024000  ; GPIO Port E
PORTF     EQU 0x40025000  ; GPIO Port F
STCTRL    EQU 0xE000E010  ; For SysTick
STRELOAD  EQU 0xE000E014  ; 
STCURRENT EQU 0xE000E018  ; 

Start
    BL GPIOINIT
Init
    BL INSERTCOIN
    B   Init

;----------------------------------------------
; INITIALIZE GPIO Ports subroutine
;  INPUT: None  OUTPUT: None
;----------------------------------------------
GPIOINIT
    PUSH {LR}
    ;------------------------------------------------------
    ; Initialize GPIO Ports
    LDR R0, =RCGCGPIO ; Enable GPIO Clock RCGCGPIO
    MOV R1, #0x36 ; Enables GPIO Port B, C, E, and F
    STR R1, [R0]  ;
    NOP           ; time for clocks to init modules
    NOP

    ;-----------------------
    ; Unlock Port F
    LDR R0, =PORTF
    LDR R1, =UNLOCK
    STR R1, [R0,#0x520] ; 

    ; Enable port F for switches
    MOV R1, #0x19       ; enable bits 0,3,4 for switches
    STR R1, [R0,#0x524] ; GPIOCR enable [0,3,4]
    STR R1, [R0,#0x510] ; GPIOPUR on [0,3,4]
    MOV R1, #0xE6       ; [0,3,4] are inputs
    STR R1, [R0,#0x400] ; GPIODIR set as above
    MOV R1, #0x19
    STR R1, [R0,#0x51C] ;GPIODEN [0,3,4]
    
    ; Enable port C for lower 4 bits of LED
    LDR R0, =PORTC
    LDR R1, =UNLOCK
    STR R1, [R0,#0x520] ; GPIOLOCK

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

;----------------------------------------------
; PRINT Subroutine
; Displays R1[9:0] on LED bar
; INPUT R1 = value to display
;  returns nothing
; PORTC[7:4] = LED[3:0]  PORTE[5:0] = LED[9:5]
; Uses temp R5 register to invert (active low LEDs)
; and move bits into proper position for port printout
;----------------------------------------------
PRINT
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR} ; Nice Stack Frame usage
    MVN R5, R1           ; Invert counter due to active low LEDs
    LDR R0, =PORTC
    LSL R10, R5, #4      ; move input[3:0] to position R10[7:4]
    STR R10, [R0,#0x3C0] ; write [7:4] into PC[7:4] (lower 4 LEDs)
    LDR R0, =PORTE
    LSR R10, R5, #4      ; move input[5:0] to position R10[5:0]
    STR R10, [R0,#0x0FC] ; write [5:0] into PE[5:0] (higher 6 LEDs)
    POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    BX LR

;----------------------------------------------
; INSERTCOIN Subroutine
;  INPUT: None   OUTPUT: None
;  Start of game, waiting for P1 and P2 Start button pushes
;  Flashes center LEDs at 2Hz until coin inserted (button push)
;  Then corresponding LED segment stays solid until both
;  players have pushed start buttons
;----------------------------------------------
INSERTCOIN
;   GLOBALS
;   R2 = OFF_State Mask of LEDs
;   R5 = ON_State Mask of LED

; Define initial OFF / ON patterns
    MOV R5, #0x30 ; Initial LED ON_pattern defined
                  ;  0000110000
    MOV R2, #0    ; Initial LED OFF_pattern defined
                  ;  0000000000

flashloop
    ; Print OFF Pattern
    MOV R1, R2;
    BL PRINT

; Input: R3: 0 == off cycle, 1== on cycle
; INPUT: R2: OFF_MASK  OUTPUT: R2: OFF_MASK (possibly updated)
; OUTPUT R1: [00], [01] P2 pushed, [10] P1 pushed, [11] tied
    MOV R3, #0
    BL Wait025s
       ; R2 OFF_MASK is modified if necessary by Wait025s as the updated off-cycle mask

    ; Print ON Pattern
    MOV R1, R5   ;  move LED On pattern to print
    BL PRINT

    MOV R3, #1
    BL Wait025s

    B    flashloop

;----------------------------------------------
; WAIT5MS Subroutine
;  Polls P1 & P2 buttons while waiting 5ms
;   Initializes and Uses SysTick
;  INPUT: None   OUTPUT: R1
;                     [1:0]==00 
;                     [1:0]==01 P2 pushed
;                     [1:0]==10 P1 pushed
;                     [1:0]==11 Both pushed
;                      all other bits are 0
;----------------------------------------------
WAIT5MS
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}    
    LDR R1, =STCTRL     ; Disable SysTick during initialization
    MOV R0, #0
    STR R0, [R1]       
    LDR R1, =STRELOAD   ; initialize to max value
    LDR R0, =0x1387F    ; (79,999+1) clock cycles @ 62.5ns / CC = 5ms
    STR R0, [R1]        ; 79999 = 0x1387F because delay 1 for setflag
    LDR R1, =STCURRENT  ; Clear current SysTick value
    MOV R0, #0
    STR R0, [R1]
    LDR R1, =STCTRL     ; Use internal clock and start counting
    MOV R0, #5         
    STR R0, [R1]

    LDR R4, =STCTRL       ; to check SysTick done flag
    MOV R6, #0            ; temp to detect P2 button push
loop
    BL P1PUSHED
    CMP R1, #0            ; CHK return value
    IT EQ
    MOVEQ R7, #2          ; P1 was pushed
    BL P2PUSHED
    CMP R1, #0            ; CHK return value 
    IT EQ
    MOVEQ R6, #1          ; P2 was pushed
    ORR R1, R7, R6;       ; Assemble the return value P1|P2
                          ; R1[1:0] = 00 (no push) 10 P1 pushed, 01 P2 pushed
    
    ; Check SysTick Flag
    LDR R5, [R4]
    ANDS R5, R5, #0x10000 ; Is SysTick done?
    BEQ loop
    POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    BX LR

;-------------------------------
; Calls WAIT5MS 50 times to equal 0.25 seconds
;   Accepts and returns OFF cycle LED flash mask
;   indicating solid LEDs for players that pushed buttons
;   Goes to Game Start if both players pushed
; INPUT:  R3: 0 == off LED flash cycle, 1== on cycle
; INPUT:  R2: OFF_MASK  (printed during LED off state)
; OUTPUT: R2: OFF_MASK (with solid LEDs for P? that pushed)
Wait025s
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}    
DELAY EQU 0x32     ; 50 * WAIT5MS = 0.25 sec
    MOV R4, #DELAY
wait025loop
    BL WAIT5MS     

    ; Modify OFF_Mask if P1 or P2 were pushed
    CMP R1, #2      ; P1 pushed
    IT EQ
    ORREQ R2, #0x20 ; change OFF mask so P1 ON and solid
    CMP R1, #1      ; P2 pushed
    IT EQ
    ORREQ R2, #0x10 ; change OFF mask to P2 ON and solid

    ; Check if currently off-cycle, if so, move generated off-cycle mask to return
    CMP R3, #0
    ITT EQ
    MOVEQ R1, R2;   ; print modified off-cycle mask immediately
    BLEQ PRINT      ; Print the OFF_MASK

    ; check if both buttons have been pushed
    CMP R2, #0x30
    BEQ STARTGAME

    SUBS R4, #1     ; Delay counter --
    BEQ exitwait025loop
    B wait025loop

exitwait025loop
    POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    BX LR
    
;----------------------------------------------
; P2PUSHED Subroutine
; Read bitbanded Port F[4] for P2 push
; INPUT: None  OUTPUT: R1[4]==0 if P1 Pushed button
;                      R1[4]==1 if not pushed
;                    all other bits are 0
;----------------------------------------------
P2PUSHED
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    LDR R4, =P2BUTTON ; PF4
    LDR R1, [R4]     
    POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    BX LR

;----------------------------------------------
; P1PUSHED Subroutine
; Read bitbanded Port F[3] for P1 push
; INPUT: None  OUTPUT: R1[3]==0 if P1 Pushed button
;                      R1[3]==1 if not pushed
;                    all other bits are 0
;----------------------------------------------
P1PUSHED
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    LDR R4, =P1BUTTON ; PF3
    LDR R1, [R4]
    POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    BX LR

;----------------------------------------------
; Wait 1 to 2 seconds random delay
; INPUT: R0 number of times to call wait 5ms
;----------------------------------------------
Wait1to2s
;    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}    
	;MOV R4, R0;     ; move input to temporary register for now
	
	; calculate
	
    ;LDR R1, =STCTRL     ; Disable SysTick during initialization
    ;MOV R0, #0
    ;STR R0, [R1]       
    ;LDR R1, =STRELOAD   ; initialize to max value
    ;LDR R0, =0x1387F    ; (79,999+1) clock cycles @ 62.5ns / CC = 5ms
    ;SUB R0, #1          ; delay-1 because systick needs 1 clk cycle to set flag
	;STR R0, [R1]        ; store the input delay parameter into reload value
    ;LDR R1, =STCURRENT  ; Clear current SysTick value
    ;MOV R0, #0
    ;STR R0, [R1]
    ;LDR R1, =STCTRL     ; Use internal clock and start counting
    ;MOV R0, #5         
    ;STR R0, [R1]

;	LDR R1, =STRELOAD
;    STR R0, [R1]        ; start counting seconds
;    LDR R1, =STCURRENT
;    MOV R0, #5
;    STR R0, [R1]        ; start counting seconds
;    LDR R1, =STCTRL
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}    
    MOV R4, R0     ; R4 = delay counter
wait1to2loop
    BL WAIT5MS     

	SUBS R4, #1     ; Delay counter --
    BEQ exitwait1to2loop
    B wait1to2loop

exitwait1to2loop
     POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    BX LR

;----------------------------------------------
; FIGHT ON!
;----------------------------------------------
; Globals
;  R4  P1 current position
;  R5  P2 current position
;  R8  Contiguous drawn move counter
STARTGAME
    ; Initialize Globals
    MOV R4, #0x20    ; P1 Initial pos (in 10-segment display)
    MOV R5, #0x10    ; P2 Initial pos (in 10-segment display)
    MOV R8, #0       ; Contiguous drawn move counter
 
STARTMOVE
; START OF A MOVE
    ; Random Delay > 1sec but less than 2 sec
    LDR R0, =STCURRENT ;
    LDR R1, [R0]       ; R1 becomes a semi random number = #
    AND R1, #3         ; examine only bits [1:0] of random value
    LDR R0, =0x509100  ; 5280000 cycles in 0.33 seconds
    MUL R1, R0         ; R1 becomes random number = 0 seconds < # CC < 1 second
    LDR R2, =0xF42400  ; #CC (16,000,000 * 62.5 ns = 1second)
    ADD R0, R2, R1     ; R0 becomes # of CC equiv to random 1 to 2 seconds {1, 1.33, 1.66, 2} seconds
	LDR R2, =0x13880   ; 80,000 in hex, b/c 5ms / 62.5ns = 80,000
	UDIV R0, R0, R2;	
    BL Wait1to2s

    ; Separate LEDS
    LSL R4, #1       ; P1 step back  R4 is P1 position
    LSR R5, #1       ; P2 step back  R5 is P2 position
    ORR R1, R4, R5   ; Present positions
    BL PRINT

; Poll both buttons for a push
READButtons    
    LDR R6, =PORTF
    LDR R1, [R6, #0x60] ; 0x60 masks both buttons
    CMP R1, #0x18       ; 0x18 No buttons yet
                        ; 0x10 PF3 P1 pushed 
                        ; 0x08 PF4 P2 pushed 
                        ; 0x00 both pushed
    BEQ READButtons

    ;0x20 if P2 pushed
    CMP R1, #0x08
    BEQ P2PUSHEDFirst

    ;0x40 if P1 pushed
    CMP R1, #0x10
    BEQ P1PUSHEDFirst

    ; 0x60 if both simulatenous (Mario always wins)
    CMP R1, #0x0
    BEQ P1PUSHEDFirst

    ; Code never gets to this point
    ; all possible scenarios branched to

;---------------------------
; P1PUSHEDFirst
P1PUSHEDFirst

; P1Moves
    LSR R4, #1       ; P1 step fwd
    ORR R1, R4, R5   ; Update Present positions
    BL PRINT

; Start Delay timer  (counter for draws)

; Delay timer equation is 2^-min(d, 4) * (320 - 80*Sn) in ms
; we have a wait5ms subtroutine, we will reuse it by calculating how 
; many times 5ms needs to be waited
; this means timer equations turns to 
; 2^-min(d, 4) * (64 - 16*Sn) times to call wait5ms
;=  CalcExp    *      R7      ; break this eqn down

    ; Calculate R7 portion by reading DIP switches
    ; Read DIP switches  
    ; PB[3:0] are dip switches
        ; PB[3:2] switches for P1
        ; PB[1:0] switches for P2
    LDR R6, =PORTB
    ; Read switches for P1 if P1 Pushed first
    LDR R7, [R6, #0x30]   ; R7 now has P1 switches [3:2]
    LSR R7, #2;           ; move to position [1:0]
    MOV R2, #16           ; 
    MUL R7, R7, R2        ; 16 * Sn
    MOV R2, #64           
    SUB R7, R2, R7        ; R7 = 64 - 16 * Sn

    ; Calculate 2^-min(d, 4)
    ;  = 1 / [2^min(d,4)]
    ;  in this case we will calculate 2^min(d,4) first
    ;  Then calculate delay by R7 / 2^min(d,4)
    ; compare draw counter and 4, call Calculate exponent f(x)
    CMP R8, #4         
    ITE LS
    MOVLS R2, R8    ; R8 < 4, so we use R8
    MOVHI R2, #4    ; R8 > 4, so we use #4 as exponent parameter
    BL CalcExp      ; returns R0 = 2^R2
    
    UDIV R0, R7, R0  ; (64 - 16 * Sn) / 2^min(d,4)
    
    BL WaitP1Delay
    CMP R0, #1     ; ONLY R0==1 mean P2 pushed in time
    BNE P1TotalWin

    ; P2 MovesInTime
    LSL R5, #1       ; P2 step fwd
    ORR R1, R4, R5   ; Update display
    BL PRINT

    ; Draw, increment draw
    ADD R8, #1
    B STARTMOVE

P1TotalWin
    LSR R4, #1       ; P1 step fwd
    ORR R1, R4, R5   ; Update display
    BL PRINT
    MOV R8, #0       ; Clear draw counter, move resolved
    BL CheckGAMEOVER
    
;----------------------------------------------
; WaitP1Delay subroutine
;  calls wait5ms a certain number of times
;  called when P1 has pushed first
; checks if P2 has pushed
; if P2 pushed before time expires, it returns
; INPUT:  R0, number of times to call wait5ms
; OUTPUT: R0, 1 == P2 pushed before timeout, 0 otherwise
;----------------------------------------------
WaitP1Delay
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}    
    MOV R4, R0   ; loop counter
    MOV R0, #0   ; default return P2 did not push in time
waitdelayloop
    BL WAIT5MS   ; 
    MOV R5, #0x3 ; 
    AND R1, R5
    CMP R1, #1   ; P2 pushed  
    BEQ exitwaitdelayloop
    SUBS R4, #1
    BEQ exitwaitdelaylooptimeout
    B waitdelayloop
exitwaitdelayloop
    MOV R0, #1             ; P2 pushed in time
exitwaitdelaylooptimeout   ; P2 did not push
    POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    BX LR

;----------------------------------------------
; Calculate exponent Subroutine
; INPUT:  R2, the exponent of 2^exp to calculate
; OUTPUT: R0, the result
;----------------------------------------------
CalcExp
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR} ; Nice Stack Frame usage
    MOV R1, #0   ; Loop counter
    MOV R4, #2   ; number to multiply
    MOV R0, #1   ; initialize result to 1
calcexploop
    CMP R1, R2      ; i <= exp?
    BEQ exitcalcexp  
    ADD R1, #1      ; increment loop ctr
    ; Body of code
    MUL R0, R0, R4 
    b calcexploop
exitcalcexp
    ; R0 has result
    POP {R4, R5, R6, R7, R8, R9, R10, R11, LR}
    BX LR


;----------------------------------------------
; P2PUSHEDFirst
P2PUSHEDFirst    
    ; P2Moves
    LSL R5, #1       ; P2 step fwd
    ORR R1, R4, R5 ; Present positions
    BL PRINT

; Start Delay timer  (counter for draws)

; Delay timer equation is 2^-min(d, 4) * (320 - 80*Sn) in ms
; we have a wait5ms subtroutine, we will reuse it by calculating how 
; many times 5ms needs to be waited
; this means timer equations turns to 
; 2^-min(d, 4) * (64 - 16*Sn) times to call wait5ms
;=  CalcExp    *      R7      ; break this eqn down

    ; Calculate R7 portion by reading DIP switches
    ; Read DIP switches  
    ; PB[3:0] are dip switches
        ; PB[3:2] switches for P1
        ; PB[1:0] switches for P2

    LDR R6, =PORTB
    ; Read switches for P1 if P1 Pushed first
       ; PB[1:0] switches for P2
    LDR R7, [R6, #0xC]    ; R7 now has P2 switches
    MOV R2, #16           ; 
    MUL R7, R7, R2        ; 16 * Sn
    MOV R2, #64           
    SUB R7, R2, R7        ; R7 = 64 - 16 * Sn

    ; compare draw counter and 4, call Calculate exponent f(x)
    CMP R8, #4         
    IT LS
    MOVLS R2, R8    ; R8 < 4, so we use R8
    MOVHI R2, #4    ; R8 > 4, so we use #4 as exponent parameter
    BL CalcExp
    
    UDIV R0, R7, R0  ; 
    
    BL WaitP2Delay
    CMP R0, #1     ; 
    BNE P2TotalWin

    ; P1 MovesInTime
    LSR R4, #1       ; P1 step fwd
    ORR R1, R4, R5   ; Redraw positions
    BL PRINT

    ; Draw, increment draw
    ADD R8, #1
    B STARTMOVE

P2TotalWin
    LSL R5, #1       ; P2 step fwd
    ORR R1, R4, R5   ; Redraw positions
    BL PRINT
    MOV R8, #0       ; Clear draw counter
    BL CheckGAMEOVER

;----------------------------------------------
; WaitP2Delay subroutine
;  calls wait5ms a certain number of times
;  called when P2 has pushed first
; checks if P1 has pushed
; if P1 pushed before time expires, it returns
; INPUT:  R0, number of times to call wait5ms
; OUTPUT: R0, 1 == P1 pushed before timeout, 0 otherwise
;----------------------------------------------
WaitP2Delay
    PUSH {R4, R5, R6, R7, R8, R9, R10, R11, LR}    
    MOV R4, R0   ; loop counter
    MOV R0, #0   ; default return P1 did not push in time
waitP2delayloop
    BL WAIT5MS
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
    CMP R1, #0x3   ; check if at one end
    BEQ GAMEOVER
    CMP R1, #0x300 ; check if at other end
    BEQ GAMEOVER
    ; Not game over, so go back to random timer for next move
    BL STARTMOVE

; Flash @ 2 Hz
GAMEOVER
    ; print LEDs ON
    ORR R1, R4, R5 ; Present positions
    BL PRINT

    ; Wait 0.25 seconds
    MOV R10, #DELAY
gameoverloop
    BL WAIT5MS
    SUBS R10, #1
    CMP R10, #0
    BNE gameoverloop

    ; print LEDs OFF
    MOV R1, #0;
    BL PRINT
    
; Wait 0.25 seconds
    MOV R10, #DELAY
gameoverloop2
    BL WAIT5MS
    SUBS R10, #1
    CMP R10, #0
    BNE gameoverloop2
    
    B GAMEOVER

       ALIGN      
       END  
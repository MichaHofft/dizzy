;------------------------------
ENTRY	EQU	5		;entry point for the cp/m bdos.
TFCA	EQU	5CH		;default file control block.        
TFCB	EQU	TFCA+1
;------------------------------
        ORG 100h
        ;
hallo:  LD A,B
        INC HL
        RL (HL)
        LD A,TFCB
        LD HL,1223H
        LD BC, buf
        JR hallo
        JP hallo

TFCC    EQU     $
TFCD    EQU     $+20

hallo2:
        LD A,(HL)
        LD (BC),A
        LD (5),A
        LD A,(IX+5h)
        LD (IY+12),A
        LD BC,(buf)
        LD HL,buf2
;
;   Routine to convert (A) into upper case ascii. Only letters
; are affected.
;
UPPER:	CP	'a'		;check for letters in the range of 'a' to 'z'.
	RET	C
	CP	'{'
	RET	NC
	AND	5FH		;convert it if found.
	RET	

buf:    DB 1,2,3,4,
buf2:   DB 5,6,7,8,9,10

buf3:    ; arithmetic test suite ... see http://benryves.com/bin/brass/manual.htm#labels
        DEFW    -1                      ; $ffff
        DEFW    -2                      ; $feff
        DEFW    17+5                    ; $16
        DEFW    17-5                    ; $0c
        DEFW    17*5                    ; $55
        DEFW    17/5                    ; $03
        DEFW    17%5                    ; $02
        DEFW    15&7                    ; $07
        DEFW    4|3                     ; $07
        DEFW    3^15                    ; $0c
        DEFW    !1                      ; $00
        DEFW    !0                      ; $01
        DEFW    ~1                      ; $feff
        DEFW    1 && 1                  ; $01
        DEFW    1 && 0                  ; $00
        DEFW    1 || 0                  ; $01
        DEFW    1 << 8                  ; $0001
        DEFW    8 >> 2                  ; $02
        DEFW    5 +  1                  ; $04
        DEFW    8 == 4*2                ; $01
        DEFW    1+2 != 2+1              ; $00
        DEFW    7 < 15                  ; $01
        DEFW    15 < 7                  ; $00
        DEFW    7 > 15                  ; $00
        DEFW    15 > -15                ; $01
        DEFW    4 <= 4                  ; $01
        DEFW    -4 <= -4                ; $01
        DEFB    33*2+4                  ; $46
        DEFB    2*(36-(2*2))/ 17 *3     ; $03
        DEFB    1,2,3,4,5,6,7
        DEFB    'Hallo \n\\\xaa\x55\r world!'

buf5:   EQU 2000h
        DEFB    34,56h,"Long term evolution\0\0"
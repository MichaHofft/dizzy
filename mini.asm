;------------------------------
ENTRY	EQU	5		;entry point for the cp/m bdos.
TFCA	EQU	5CH		;default file control block.        
TFCB	EQU	TFCA+1
;------------------------------
        ORG 100h
        ;
hallo:  ; RRC further
        LD      IX,buftmp       ; not a constant one
        LD      A,00110001b
        LD      (IX+3),A
        OR      A               ; clears carry?
        RRC     (IX+3)          ; shall by CY, $98
        LD      B,(IX+3)
        NOP
        NOP
        ; RR further
        LD      HL,buftmp       ; not a constant one
        LD      A,11011101b
        LD      (HL),A
        OR      A               ; clears carry?
        RR      (HL)            ; shall by CY, $6e
        LD      B,(HL)
        NOP
        NOP
        ; RL further
        LD      IY,buftmp       ; not a constant one
        LD      A,10001111b
        LD      (IY+4),A
        RL      (IY+4)          ; shall by CY, $1e
        LD      B,(IY+2)
        NOP
        NOP
        LD      B,10001000b
        RL      B               ; shall be CY, %00010001 = $11
        NOP
        LD      HL,buftmp       ; not a constant one
        LD      A,11001100b
        LD      (HL),A
        RL      (HL)            ; shall by CY, $99
        LD      A,(HL)
        NOP
        ; RLC further
        LD      IY,buftmp       ; not a constant one
        LD      A,11001100b
        LD      (IY+4),A
        RLC     (IY+4)          ; shall by CY, $99
        LD      B,(IY+2)
        NOP
        NOP
        LD      B,10001000b
        RLC     B               ; shall be CY, %00010001 = $11
        NOP
        LD      HL,buftmp       ; not a constant one
        LD      A,11001100b
        LD      (HL),A
        RLC     (HL)            ; shall by CY, $99
        LD      A,(HL)
        NOP
        ; RRA
        LD      A,11100001b
        OR      A
        RRA                     ; shall be CY, %01110000 = $70
        NOP
        ; RRCA
        LD      A,00010001b
        RRCA                    ; shall be CY, %10001000 = $88
        NOP
        ; RLA
        LD      A,01110110b
        SCF
        RLA                     ; shall be no CY, %11101101 = $ed
        NOP
        ; RLCA
        LD      A,10000001b
        RLCA                    ; shall be CY, %00000011 = $03
        NOP
        ; XOR
        LD      A, $55
        LD      B, $aa
        XOR     B               ; shall be $ff
        XOR     $01             ; shall be $fe, Signed
        LD      IX,buf7         ; is 43, 44, 45, 255
        XOR     (IX+3)          ; $01
        NOP
        ; AND
        LD      A, $ff
        LD      B, $aa
        AND     B               ; shall be $aa
        AND      $22            ; shall be $22
        LD      HL,buf2         ; is 5
        AND      (HL)           ; $00, Zero
        NOP
        ; OR
        LD      A, $55
        LD      B, $aa
        OR      B               ; shall be $ff
        LD      A, $11
        OR      $88             ; shall be $99
        LD      HL,buf2         ; is 5
        OR      (HL)            ; $99+5 = $9d
        SCF                     ; CY is 1
        OR      A               ; still $9d, but CY is 0
        NOP
        ; ADD
        LD      A,42
        LD      B,A
        ADD     A,B             ; 84, kein Carry
        LD      C,230
        ADD     A,C             ; 84 + 230 -> 58, Carry!
        ; SUB        
        LD      A,58
        LD      D,55
        SUB     D               ; shall be 3, 
        SUB     3               ; shall be 0, Sign
        LD      IX,buf          ; buf = 1,2,3,4..
        SUB     (IX+0)          ; A shall be $ff
        LD      IY,buf
        SUB     (IY+1)          ; A shall be $fd
        ; SBC (with Carry)
        LD      A,59
        LD      D,55
        SCF                     ; set CY
        SBC     A,D             ; shall be 3, 
        SCF                     ; set CY
        SBC     A,3             ; shall be ff, Carry (Borrow!)
        LD      IX,buf          ; buf = 1,2,3,4..
        SBC     A,(IX+0)        ; A shall be $ff
        LD      IY,buf
        SBC     A,(IY+1)        ; A shall be $fd
        ADD     A,42            ; shall be 42 again
        LD      HL,buf7
        ADD     A,(HL)          ; 85
        LD      IX,buf7
        ADD     A,(IX+2)        ; 85 + 45 = 130
        NOP
        LD      IX,buf
        LD      C,(IX+1)
        LD      HL,buf2
        LD      (HL),C
        LD      D,(HL)
        LD      (IX+-2),D
        LD      (HL),$5a
        LD      (IX+-4),$a5
        LD      A,(BC)
        LD      A,11
        LD      A,(buf2+3)
        LD      A,$55
        LD      BC,buf
        LD      (BC),A
        LD      A,$77
        LD      DE,buf
        LD      (DE),A
        LD      A,$99
        LD      (buf),A
        LD      A,$32
        LD      I,A
        LD      A,$43
        LD      R,A
        LD      A,I                     ; A to be $32
        LD      A,R                     ; A to be $43
        LD      IX,buf
        LD      IY,$55aa
        LD      HL,(buf4)               ; HL to be $200
        LD      BC,(buf)                ; BC to be $0201
        LD      DE,(buf+1)              ; DE to be $0302
        LD      SP,(buf+2)              ; SP to be $0403
        LD      IX,(buf+3)              ; SP to be $0504
        LD      IY,(buf+4)              ; SP to be $0605
        LD      (buf6),HL
        LD      (buf+1),BC
        LD      (buf+2),DE
        LD      (buf+3),SP
        LD      (buf+4),IX
        LD      (buf+5),IY
        LD      SP,HL
        LD      SP,IX
        LD      SP,IY
        LD      SP,spend
        PUSH    BC
        PUSH    DE
        PUSH    HL
        PUSH    AF
        PUSH    IX
        PUSH    IY
        POP     BC
        POP     IX
        NOP
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

        ORG $200

buf:    DB 1,2,3,4,
buf2:   DB 5,6,7,8,9,10

buf4:   DEFW $200

buf6:   DEFW $5aa5
        DEFW $a55a
        DEFW $1234
        DEFW $4321

buf7:   DB      43
        DB      44
        DB      45
        DB      255

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

        ORG $600

buftmp: DEFB    0,0,0,0

spstrt: DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
spend:  DEFB 255
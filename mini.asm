;------------------------------
ENTRY	EQU	5		;entry point for the cp/m bdos.
TFCA	EQU	5CH		;default file control block.        
TFCB	EQU	TFCA+1
;------------------------------
        ORG 100h
        ;
hallo:  ; START
        LD      A,$00
        LD      B,$aa
        SET     0,B
        SET     7,A
        ;
        LD      IY,buf
        LD      (IY+2),$aa
        BIT     3,(IY+2)
        BIT     2,(IY+2)
        NOP
        ;
        LD      IX,buf
        LD      (IX+2),$55
        BIT     3,(IX+2)
        BIT     2,(IX+2)
        NOP
        ; BIT TEST
        LD      HL,buf
        LD      (HL),$55
        BIT     3,(HL)
        BIT     2,(HL)
        NOP
        ;
        LD      A,$aa
        BIT     2,A
        BIT     3,A
        BIT     0,A
        LD      B,A
        BIT     1,B
        ; INC RR
        LD      IY,$aa55
        INC     IY
        DEC     IY
        ; --
        NOP 
        ; INC/ DEC
        LD      IY,buf
        LD      (IY+2),$55
        INC     (IY+2)
        DEC     (IY+2)
        NOP
        LD      IX,buf
        LD      (IX+2),$55
        INC     (IX+2)
        DEC     (IX+2)
        NOP
        LD      HL,buf
        LD      (HL),$aa
        INC     (HL)
        DEC     (HL)
        NOP
        LD      B,$00
        INC     B
        DEC     B
        DEC     B
        NOP
        ; CP
        LD      A,$70
        LD      B,$70
        CP      B               ; compare A with B, shall have Z set
        LD      B,$91
        CP      B
        CP      $70
        CP      $90
        ;
        LD      IX,buf
        LD      (IX+2),$70
        CP      (IX+2)
        LD      (IX+2),$90
        CP      (IX+2)
        ;
        LD      IY,buf
        LD      (IY+4),$70
        CP      (IY+4)
        LD      (IY+4),$90
        CP      (IY+4)
        ;
        LD      HL,buf
        LD      (HL),$70
        CP      (HL)
        LD      (HL),$90
        CP      (HL)
        ; EX (SP),IY
        LD      SP,10           ; SP is 0x0a
        LD	    BC,$1234
        PUSH	BC              ; SP is 0x08 (0x09 is $12, 0x08 is $34)
        POP	    DE	            ; SP shall be reset again to 0x0a
        PUSH    BC              ; SP is 0x08
        LD	    IY,$55aa
        EX (SP),IY              ; reads 0x08, 0x09, will end up 0x0a
        NOP	                    ; ASSERT H = $12, L = $34
        EX (SP),IY
        NOP	                    ; ASSERT H = $55, L = $aa
        ; EX (SP),IX
        LD      SP,10           ; SP is 0x0a
        LD	    BC,$1234
        PUSH	BC              ; SP is 0x08 (0x09 is $12, 0x08 is $34)
        POP	    DE	            ; SP shall be reset again to 0x0a
        PUSH    BC              ; SP is 0x08
        LD	    IX,$55aa
        EX (SP),IX              ; reads 0x08, 0x09, will end up 0x0a
        NOP	                    ; ASSERT H = $12, L = $34
        EX (SP),IX
        NOP	                    ; ASSERT H = $55, L = $aa
        ; EX (SP),HL
        LD      SP,10           ; SP is 0x0a
        LD	    BC,$1234
        PUSH	BC              ; SP is 0x08 (0x09 is $12, 0x08 is $34)
        POP	    DE	            ; SP shall be reset again to 0x0a
        PUSH    BC              ; SP is 0x08
        LD	    HL,$55aa
        EX (SP),HL              ; reads 0x08, 0x09, will end up 0x0a
        NOP	                    ; ASSERT H = $12, L = $34
        EX (SP),HL
        NOP	                    ; ASSERT H = $55, L = $aa
        ; EXX
        LD      BC,$445a
        LD      DE,$3da2
        LD      HL,$8859
        EXX
        NOP                     ; ASSERT B = $00, C = $00, D = $00, E = $00, H = $00, L = $00
        LD      BC,$0988
        LD      DE,$9300
        LD      HL,$00e7
        EXX
        NOP                     ; ASSERT B = $44, C = $5a, D = $3d, E = $a2, H = $88, L = $59
        EXX
        NOP                     ; ASSERT B = $09, C = $88, D = $93, E = $00, H = $00, L = $e7
        ; EX AF,AF'
        LD      A,$55
        SCF
        EX      AF,AF'
        NOP                     ; ASSERT A = $00, CY clear
        LD      A,$aa
        EX      AF,AF'
        NOP                     ; ASSERT A = $55, CY set
        EX      AF,AF'
        NOP                     ; ASSERT A = $aa, CY clear
        ; EX DE.HL
        LD      DE,$1234
        LD      HL,$5678
        EX      DE,HL
        NOP                     ; ASSERT D = $56, E = $78, H = $12, L = $34
        ; RRD
        LD      HL,buftmp
        LD      (HL),00100000b
        LD      A,10000100b
        RRD
        LD      B,(HL)          ; ASSERT B = $42
        LD      C,A             ; ASSERT C = $80
        ; RLD
        LD      HL,buftmp
        LD      (HL),00110001b
        LD      A,01111010b
        RLD
        LD      B,(HL)          ; ASSERT B = $1a
        LD      C,A             ; ASSERT C = $73
        ; SRL in multiple ways
        OR      A               ; clear carry
        LD      A,10001111b
        SRL     A
        LD      B,A
        NOP                     ; ASSERT B = $47, Cy set
        OR      A               ; clear carry
        LD      A,10001111b
        LD      HL,buftmp       ; not a constant one
        LD      (HL),A
        SRL     (HL)
        LD      C,(HL)
        NOP                     ; ASSERT C = $47, Cy set
        OR      A               ; clear carry
        LD      A,10001111b
        LD      IY,buftmp       ; not a constant one
        LD      (IY+1),A
        SRL     (IY+1)
        LD      D,(IY+1)
        NOP                     ; ASSERT D = $47, Cy set
        ; SRA in multiple ways
        OR      A               ; clear carry
        LD      A,10111000b
        SRA     A
        LD      B,A
        NOP                     ; ASSERT B = $dc, Cy cleared
        OR      A               ; clear carry
        LD      A,10111000b
        LD      HL,buftmp       ; not a constant one
        LD      (HL),A
        SRA     (HL)
        LD      C,(HL)
        NOP                     ; ASSERT C = $dc, Cy cleared
        OR      A               ; clear carry
        LD      A,10111000b
        LD      IY,buftmp       ; not a constant one
        LD      (IY+1),A
        SRA     (IY+1)
        LD      D,(IY+1)
        NOP                     ; ASSERT D = $dc, Cy cleared
        ; SLA in multiple ways
        OR      A               ; clear carry
        LD      A,10110001b
        SLA     A
        LD      B,A
        NOP                     ; ASSERT B = $62, Cy set
        OR      A               ; clear carry
        LD      A,10110001b
        LD      HL,buftmp       ; not a constant one
        LD      (HL),A
        SLA     (HL)
        LD      C,(HL)
        NOP                     ; ASSERT C = $62, Cy set
        OR      A               ; clear carry
        LD      A,10110001b
        LD      IY,buftmp       ; not a constant one
        LD      (IY+1),A
        SLA     (IY+1)
        LD      D,(IY+1)
        NOP                     ; ASSERT D = $62, Cy set
        ; RRC further
        LD      IY,buftmp       ; not a constant one
        LD      A,00110001b
        LD      (IY+3),A
        OR      A               ; clears carry?
        RRC     (IY+3)          ; shall by CY, $98
        LD      C,(IY+3)
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

        ORG $400

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
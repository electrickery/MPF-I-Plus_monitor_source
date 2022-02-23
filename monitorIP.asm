;*********************************************************
;*                                                       *
;*    COPYRIGHT, MULTITECH INDUSTRIAL CORP. 1983         *
;*    ALL right reserved.                                *
;*    No part of this software maybe copied without      *
;*    the express written consent of MULTITECH           *
;*    INDUSTRIAL CORP.                                   *
;*                                                       *
;*********************************************************
;
;
;
;
;
P82551: EQU	83H     ;8255 I control port
DIG1:   EQU	80H     ;8255 I port A
DIG2:   EQU	81H     ;8255 I port B
DIG3:   EQU	82H     ;8255 I port C
P82552: EQU	93H     ;8255 II Control port
SEG1:   EQU	90H     ;8255 II port A
SEG2:   EQU	91H     ;8255 II port B
KIN:    EQU	92H     ;8255 II port C
PWCODE: EQU	0A5H    ;Power up code
ZSUM:   EQU	0D5H    ;This will make the sum of all
                    ;monitor codes to be zero.

; The following EQUATES are used for timing. Their values
; depend on the CPU clock frequency. (In this version, the
; crystal freqency is 1.79MHz.)

COLDEL: EQU     80  ; Column delay for routine
                    ; SCAN and SCAN1.
F1KHZ:  EQU     65  ; Delay count for 1K Hz square wave,
                    ; used by routine TONE1K.
F2KHZ:  EQU     31  ; Delay count for 2K Hz square wave,
                    ; used by routine TONE2K.
MPERIOD:EQU     32  ; 1KHz and 2KHz threshold used by
                    ; tape input routine PERIOD.

; The following EQUATEs are used for tape modulation.
; It the quality of tape recoder is good, the user may
; change'4 4 2 8' to '2 2 1 4'. This will double
; the tape data rate.
; If the quality of tape recoder is poor, the user may
; change '4 4 2 8' to '6 6 3 12'. This will improve
; error performance but slow down the data rate.
; Although the data format is changed, the tape is still
; compatible in each case, because only the ratio is
; detected in the Tape read.

ONE_1K: EQU     4
ONE_2K: EQU     4
ZERO_1K:EQU     2
ZERO_2K:EQU     8

;**************************************************************
;I/0 port assignment: (8255 I)
;                                                             ----P2----
; port A (address 80H): The first eight digits of display
; bit0--digit 1
; bit1--digit 2
; bit2--digit 3
; bit3--digit 4
; bit4--digit 5
; bit5--digit 6
; bit6--digit                   ;
; bit7--digit 8
; Port B (Address 81H): The second eight digits of display
; bit0--diqit9
; bit1--digit10
; bit2--digit11
; bit3--digit12
; bit4--digit13
; bit5--digit14
; bit6--digit15
; bit7--digit16
; port C (address 82H): The last four digits of display
; bit0--digit17
; bit1--digit18
; bit2--digit19
; bit3--digit20
; bit4--SHIFT key
; bit5--CTRL key
; bit6,7--unused

;**************************************************************
;I/0 port assignment: (8255 II)

; port A (address 90H):The first eight segments.
; bit0--segment a
; bitl--segment b
; bit2--segment c
; bit3--segment d
; bit4--segment e
; bit5--segment f
; bit6--segment g
; bit7--segment h
; port B (address 91H): The second seven segments.
; bit0--segment i
; bit1--segment j
; bit2--segment k
; bit3--segment l
; bit4--segment m
; bit5--segment n
; bit6--segment dp
; bit7:unused
; port C (address 92H): TapeI/0, Break, Keyboard input.
; bit0--K1 (Keyboard matrix 1st row)
; bit1--K2 (keyboard matrix 2nd row)
; bit2--K3 (Keyboard matrix 3rd row)
; bit3--EAR (Input from tape recoder)
; bit4--Break signal
; bit5--MIC (Output to taperecoder)
; bit6--7:unused

; ************************************************************** ----page 2 ---
; --reset--
; There are two cases that will generate a RESTE ?.?
;    (i) power-up
;   (ii) 'RS' key pressed
; In both cases, the following actions will be taken:
;   a) disable interrupt, set interrupt mode to 0
;      set I register to 00 and start execution
;      at address 0000 (by Z80 CPU itself).
;   b) set user's SP to FEA0H;
;   c) set user's I register to 00 and disable user's
;      interrupt flip-flop;
; In addition, subroutine INI will be called on power-up
; reset, which has the following effects:
;    d) turn on printer (PRT_MPF)
;    e) disable BREAKPOINT;
;    f) set the contents of location FF00H FF01H to 66 and
;       and 00 respectively. This will make instruction RST
;       38H (opcode FF) have the same effect as BREAK.
;    g) set the default value of EDITOR and ASSEMBLER.
;    h) set the limit address of INSERT and DELETE.
; Memory location POWERUP is used to distinguish power-up
; from RS-key. (POWERUP) contains a random data when
; power-up and contains PWCODE (0A5H) there after.

        LD      BC,300H         ;Power_up delay
RS_START:
        CPD
        JP      PE,RS_START
        
; Initial 8255 I to mode 0 with port A and B output ,port C
;pc0-pc3:out pc4-pc7:in.The control word is 88H.

        LD      A,10001000B
        OUT    (P82551),A

;Intial 8255 II to mode 0 with port A and B output ,port C
;pc0-pc3:in pc4-pc7:out.The control word is 81H.

        LD      A,10000001B
        OUT     (P82552),A

; When the control word is sent out to 8255, all output
; ports a recleared to 0.It is necessary to disable
; BREAK and deactivate all I/0 by sending 0FFH to
; 8255 I portA,B,C and 8255II port C.

        LD      A,0FFH
        OUT     (KIN),A     ;Disable the BREAK signal.
        OUT     (DIG1),A    ;Disable all digits.
        OUT     (DIG2),A
        OUT     (DIG3),A
        LD      SP,SYSSTK   ;Initial system stack.
        
; If the content of location POWERUP is not equal to
; PWCODE, call subroutine INI.Continue otherwise.

        LD      A,(POWERUP)
        CP      PWCODE          ;                         ----page 3----
        CALL    NZ,INI          ;Cold start

; Address 28H and 30H are reserved for BREAK (RST28H)
; and software BREAK (RST30H). Skip these area, monitor
; program resumes at RESET1.

        JR      RESET1
        DEFB    0FFH 
;
;**************************************************************
        ORG     28H
; Address 28H is the entry point of BREAK trap.
; If a location is set as a BREAK point, the monitor
; will change the content of this location to C7 (RST28H)
; before transfering control to user's program.
; In execution of user's program, a trap will occur if
; user's PC passes this location. The monitor then takes
; over control and the content of BREAK address
; will be restored. Monitor takes care of everything
; and makes the whole mechanism transparent to the user.
; The return address pushed on to stack is the PC after
; executing RST28H. The original break address should
; be one less than that. The following 3 instructions
; decrease the content of (SP) by one without changing
; HL.

        EX      (SP),HL
        DEC     HL
        EX      (SP),HL
        LD      (HLTEMP),HL
        JR      CONT28
;
;**************************************************************
        ORG	30H

; Instruction RST 30H (opcode F7) is usually used as:
;   i) Software break;
;  ii) Terminator of user's program.
; The effect of this instruction is to save all user's
; registers and return to monitor.

        JR      NMI

;**************************************************************
; This is a part of reset routine. Address 0028 and
; 0030 are reserved for break point. Reset routine
; skips this area and resumes here.
;
RESET1:
        LD      HL,0
        JR      RESET2

;**************************************************************

;The following byte makes the sum of the monitor
;code in ROM zero.

        DEFB    ZSUM
;        DEFB    0E5h        ; filler?
                            ;                             ---page 4 ----
;**********************************************************

        ORG     38H

; Entry point of RST38H (opcodFF) or mode1 interrupt.
; Fetch the addresss to redin location FF00 and FF01,
; then jump to this address. Initially, FF00 and FF01
; are set to 0066. So RST 38 will have the same effect
; as software break. By changing the content of FF00
; and FF01, the user can define his or her own service
; routine.
; The next three instructions push the contents of FF00
; and FF01 to stack without changing any registers.

        PUSH    HL
        LD      HL,(IM1AD)  ;Initially stored 0066H.
        EX      (SP),HL

; The top of the stack is now the address of user
; defined service routine.Pop out this address then
; branchtoit.

        RET
;
;*************************************************************
CONT28:
; This is a part of break service routine. It continues
; the program at RST28.

        LD      (ATEMP),A

; The monitor has changed the content of user's
; program at break address.Then ext3 instructions
; restored the destroyed content. BRAD contains the
; break address, BRDA contains the original data at
; break address.

        LD      HL,(BRAD)
        LD      A,(BRDA)
        LD      (HL),A
; Send break enable signal to hardwr&counte1.
; A nonmaskable interrupt will be issued at thethMi's.

        LD      A,11101111B
        OUT     (KIN),A
        LD      A,(ATEMP)   ; 1st M1
        LD      HL,(HLTEMP) ; 2nd M1
        NOP                 ; 3rd M1
        RET                 ; 4th M1

; Return to user's program. Execute the 1nstruct100
; at break address, After finishing one instruction,
; a nonmaskable interrupt happensnd then returns
; to the monitor program again.
;
RESET2:
        LD      (USERIF),HL ; Set user's I register and
                            ; interupt flip flop to H   ---- page 5 ----
        LD      (TEST),HL   ; Set the contents of TEST and
                            ; STEP BF to be zero.

; TEST is a flag for the use of monitor itself. Illegal key-in
; blanking (bit 7 of TEST) and automatic leading zero
; (bit 0) use this flag. Clear it here.

        LD      HL,USERSTK
        LD      (USERSP),HL
        CALL    INI7
        SCF

; Address 66H is the address for non maskable interrupt.
; Skip this area, monitor resumes at SETST0

        JR      SETST0
;
;**************************************************************
        ORG     66H
NMI:

; Entry point of non maskable interrupt. NMI will occur
; when user's program is breaked.
; The service routine which starts here saves all
; user's registers and status. It also check the validity
; of user's SP.

        LD      (ATEMP),A       ;SaveAregister
        LD      A,0FFH  ;DisableBREAKsignaland all digits.
        OUT     (DIG1),A
        OUT     (DIG2),A
        OUT     (DIG3),A
        OUT     (KIN),A
        LD      A,(ATEMP)       ;Restore A register
RGSAVE: LD      (HLTEMP),HL     ;Save register HL
        POP     HL              ;Get return address from stack
        LD      (USERPC),HL     ;Set user's PC to return
                                ;address
        LD      HL,(HLTEMP)     ;Restore HL register
        LD      (USERSP),SP     ;set user-SP to current SP
        LD      SP,USERIY+2     ;Save other registers by
        PUSH    IY              ;continuously pushing them
        PUSH    IX              ;on to stack
        EXX
        PUSH    HL
        PUSH    DE
        PUSH    BC
        EXX
        EX      AF,AF'
        PUSH    AF
        EX      AF,AF'
        PUSH    HL
        PUSH    DE
        PUSH    BC
        PUSH    AF
        
; The next two instructions save I register.
; The interrupt flip-flop (IFF2) is copied into
; parity flag (P/V) by instruction LDA, I.              ---- page 6 ----
; The interrupt status (enabled or disabled)
; can be determined by testing parityf lag.

        LD      A,I
        LD      (USERIF+1),A

; The next four instructions save IFF2 into
; user's IFF.
        LD      A,0
        JP      PO,SETIF        ;PO--P/V=0
        LD      A,1
SETIF:  LD      (USERIF),A
;

        LD      SP,SYSSTK       ;Set SPtosystem stack.
        
; The next 7 instructions check user's SP.
; If the user's SP points to a location not
; in RAM, display ERR-SP.

        LD      HL,(USERSP)
        DEC     HL
        CALL    RAMCHK
        JR      NZ,SETST2
        DEC     HL
        CALL    RAMCHK
        JR      NZ,SETST2

; If the user's stack and system stack are
; overlayed, then display SYS-SP. This checking
; is done by the following instructions.

        LD      DE,-USERSTK+1
        ADD     HL,DE
        JR      C,SETST3
SETST0:
        LD      A,(BRDA)
        LD      HL,(BRAD)	;Restore the data at break point
                            ;address.
        LD      (HL),A

; In execution of STEP or GO command, if the
; user's SP is legal (carry flag is zero) then
; display user's PC and the first four
; register contents.
; User can use the UP or DOWN keys to check
; the register contents.
; Otherwise, display fixed message (ERR-SP
; or SYS-SP)

        CALL    NC,MEMDP2
;
;
;**************************************************************
; Scan the display and key board. When a key is
; detected, take proper action according to the
; key pressed.
;                                                       ---- page 7 ----
MAIN:
        LD      SP,SYSSTK       ;Initialsystemstack.
        CALL    SCAN            ;Scandisplayandinputkeys.
                                ;RoutineSCANwillnotreturn
                                ;untilanykeyispressed.
                                ;Aftera keyisdetected,there
                                ;willbeaccompaniedwitha
                                ;beepsound.
        PUSH    AF
        CALL    CLRBF
        POP     AF
        CALL    KEYEXEC         ;Inputkeydispatchroutine.
        JR      MAIN            ;BacktoMAIN,getmorekeys
                                ;andexecutethem.
;
;**************************************************************

SETST2:
        LD      HL,ERR_SP       ;DisplayERR_SP
        JR      SETST4
SETST3:
        LD      HL,SYS_SP       ;DisplaySYS_SP
SETST4:
        CALL    PRTMES          ;Printmessage
        SCF
        JR      SETST0

;**************************************************************

;SOFTWAREESCcommand--Reentermontior.
;ExecutedbydepressingtheQandCTRLkeystogether.
;TheESCcommandescapesfromtheexistingcommand
;andreturns tomonitor.
;ESCisoperativeonlyinthecommandsthatsample
;thekepboard.
;MPFIPwillrespondstoESCbydisplayingtheMPFIP
;montiorprompt<.

ESCAPE:
        LD      SP,SYSSTK
        CALL    CLRBF
        CALL    CR3
        JR      MAIN

;**************************************************************

;ExecutedwheUParrow or DOWNarrowkeyispressed.454

FOR:
        LD      A,(TYPEFG)
        CP      10H         ;M
        JP      Z,MFOR      ;Displaynextfourmemory
                            ;contents.
        JP      NC,RFOR     ;Displaynextfourregister
                            ;contents.
        JR      IGNORE
BACK:   
        LD      A,(TYPEFG)  ;                           ---- page 8 ----
        CP      10H         ;M
        JP      Z,MBACK     ;Display last four memory
                            ;contents.
        JP      NC,RBACK    ;Display last four register
                            ;contents.
        JR      IGNORE

;**************************************************************

;Input key dispatch routine.
;This routine uses the key codes returned by subroutine
;SCAN, which is one byte (ASC II code) stored in A.

KEYEXEC:
        CP      69H
        JR      Z,FOR           ;DOWN ARROW.
        CP      5EH
        JR      Z,BACK          ;UP ARROW.
        CP      'M'         
        JP      Z,MEMEXC        ;MEMORY DISPLAY AND MODIFY.
        CP      'R'
        JP      Z,REGEXC        ;REGISTER DISPLAY AND MODIFY.
        CP      'L'
        JP      Z,LOAD          ;TAPE READ.
        CP      'W'
        JP      Z,DUMP          ;TAPE WRITE
        CP      'G'
        JP      Z,GOEXEC        ;EXECUTION
        CP      'S'
        JP      Z,STEP          ;SINGLE STEP.
        CP      'B'
        JP      Z,BREAK         ;BREAK AT A SPECIFIED ADDRESS.
        CP      'F'
        JP      Z,FILLDA        ;FILL DATA.
        CP      'I'
        JP      Z,INSET         ;INSERT A BLOCK OF DATA.
        CP      'D'
        JP      Z,DELETE        ;DELETE ONE BYTE OF DATA.
        CP      'J'
        JP      Z,JUMP          ;JUMP RELATIVE.
        CP      1
        JP      Z,ASM           ;ASSEMBLER (CONTROL A).
        CP      0CH
        JP      Z,LASM          ;LINE ASSEMBLER (CONTROL L).
        CP      2
        JR      Z,BASIC3        ;ENTER BASIC (CONTROL B).
        CP      3
        JR      Z,BASIC3        ;REENTER BASIC (CONTROL C).
        CP      4
        JR      Z,DEASM3        ;DISASSEMBLER (CONTROL D).
        CP      5
        JP      Z,EDIT          ;EDITOR (CONTROL E).
        CP      7
        JR      Z,BEEP_CONTROL  ;BEEP SOUND CONTROL (CONTROL G).
        CP      12H
        JP      Z,REEDIT        ;REEDIT (CONTROL R).
        CP      10H
        JR      Z,PRT_CONTROL   ;PRINTER CONTROL (CONTROL P).  ---- page 9 ----
        CP      0DH
        JP      Z,CR3           ;LINEFEED.
        
;**************************************************************

IGNORE:
        LD      HL,TEST
        SET     7,(HL)      ;Routine SCAN will check
                            ;bit 7 of test. If it is set
                            ;all DIGITS will be disabled.
                            ;This is a warning message to
                            ;the user when a illegal key
                            ;is entered.
        RET

;**************************************************************

;Executedbydepressingthe D and CTRL keys together.
;Sincethedisassemblerislocatedonthemontior
;ofprinter,sothatMPFIPwillignorecommandD
;unlessprinter(PRT_MPF)isexists.

DEASM3:
        CALL    PTESTT      ;Ret if printer is not exists
                            ;or the toggle printer switch is off.
        RET     NZ
        CALL    DEASM3      ;Call disassembler.
        RET
        
;**************************************************************

;Executedbydepressing B and CTRL keys together.
;TheoptionalMPFIPBASICINTERPRETERisaBKROM
;resident.Itissuppliedasone2764ROM thatplugs
;intosocketU3.
;ThestartingaddressofBASICINTERPRETERis2000H
;MPFIPwillcheckthecontentofthememorylocation
;2000His.0CDHornot.IfyesenterBASIC,otherwise
;ignorethiscommandandreturntomonitor.
;AvoidtochangingthecontentsinRAM ,weusedthe
;commandCtoreenterBASIC.

BASIC3:
        LD      B, A
        LD      A,(2000H)
        CP      0CDH
        LD      A,B
        RET     NZ
        CP      2
        JP      Z,BASICZ
        JP      BASICC
        
;**************************************************************

; Control print command -- Toggle printer on/off
; The CTRL PRINT command turns the printer on
; if it is off and off if it is on.
; The command is entered by depressing the P and CTRL  ---- page 10 ----
; keys together.

PRT_CONTROL:
        LD      A,(PRTFLG)
        CPL
        LD      (PRTFLG),A
        AND     A
        LD      HL,PRTOFF
        JR      NZ,PRTF
        LD      HL,PRTON
PRTF:
        CALL    PRTMES
        RET

;
;**************************************************************

; Control sound command--Toggleswitchon/off

BEEP_CONTROL:
        LD      HL,BEEPSET
        LD      A,(HL)
        CPL
        LD      (HL),A
        RET

;**************************************************************

;Power_upintialization.

INI:
        LD      HL,0
        LD      (PRTFLG),HL     ;Set toggle printer
                                ;switchon.
                                ;Set togglesoundbeep
                                ;switchon.

;The next7 instructionscheck IC onU4isRAMornot.

        LD      HL,0F7FFH
RAMT1:  LD      BC,8000H
RAMT2:  CALL    RAMCHK
        JR      Z,TNEXT
        JR      INI8
TNEXT:   CPD
        JP      PE,RAMT2

;Thenext.four instructionssetthedefaultvaluesaccording
;toEDITORand ASSEMBLERrespectively.

        LD      HL,RAM4K_VALUE_SET
INI6:
        LD      DE,RAM_START_ADDR
        LD      BC,12
        LDIR
        
        CALL    INI7        ;Getresetdisplaypattern.
        LD      IX,DISPBF   ;                           ---- page 11 ----
                            ;Displaythefollowing
                            ;patternssequence,each0.157
                            ;seconds:
                            ;       '                   *'
                            ;       '                  **'
                            ;       '                 ***'
                            ;       '                ****'
                            ;       '               *****'
                            ;       '              *****M'
                            ;       '             *****MP'
                            ;       '            *****MPF'
                            ;       '           *****MPF-'
                            ;       '          *****MPF-I'
                            ;       '         *****MPF-I-'
                            ;       '        *****MPF-I-P'
                            ;       '       *****MPF-I-PL'
                            ;       '      *****MPF-I-PLU'
                            ;       '     *****MPF-I-PLUS'
                            ;       '    *****MPF-I-PLUS*'
                            ;       '   *****MPF-I-PLUS**'
                            ;       '  *****MPF-I-PLUS***'
                            ;       ' *****MPF-I-PLUS****'
                            ;       '*****MPF-I-PLUS*****'
        LD      C,20        ;Pattern counts.
INI1:   LD      B,10                ;Display 0.157 sec.
INI2:
        CALL    SCAN1
        DJNZ    INI2
        INC     IX
        INC     IX
        DEC     C
        JR      NZ,INI1
        CALL    PTESTT
        JR      NZ,INI5
        CALL    PRT_MPF     ;Print '*****MPF-I-PLUS*****'
INI5:   LD      A,PWCODE
INI3:   LD      (POWERUP),A ;Load power_up code into
                            ;(POWERUP). The monitor
                            ;uses the location to decide
                            ;whether a reset signal is
                            ;on power_up.
        LD      HL,FBEEP
        LD      (HL),44H    ;Frequency of BEEP.
        INC     HL
        LD      (HL),2FH    ;Time duration of BEEP.
        INC     HL
        LD      (HL),0
INI4:   LD      HL,NMI
        LD      (IM1AD),HL  ;Set the service routine
                            ;of RST 38H to NMI ,which is the
                            ;nonmaskable interrupt service
                            ;routine for break point and
                            ;single step.
        LD      L,0E6H      ;Set SOFTWARE ESCAPE address
                            ;to be 00E6H.
                            ;(i.e.,User'sprogram return
                            ;address.)
        LD      (USERSTK),HL               ;           ---- page 12 ----
        CALL    CLRI
CLRB:
;Clearbreakpointbysettingthebreakpointaddress
;to1FFFH.Thisaddressisthelastaddressofmonitor
; so,breakcanneverhappen.

        LD      HL,1FFFH
        LD      (BRAD),HL
        RET
CLRI:
; ClearlimitaddressofINSERTandDELETEcommand.
; Avoid to changing the contents of SYSTEM RAM ,we must
; setlimitaddress.
; The default value of limit address is 0FE00H.

        LD      HL,0FE00H
        LD      (END_ADDR),HL
        RET
INI7:
; Get pattern '                    *****MPF-I-PLUS*****'

        CALL    CLRDSP
        LD      HL,INPBF+20
        LD      (OUTPTR),HL
        LD      HL,DISPBF+40
        LD      (DISP),HL
        LD      HL,MPFII
        CALL    MSG
        LD      IX,DISPBF+40
        LD      HL,0
        LD      (TYPEFG),HL     ;Set the contents of TYPEFG and
                                ;CRSET to zero.
        CALL    CR0
        RET
INI8:
        LD      HL,RAM2K_VALUE_SET
        JR      INI6
;
;**************************************************************
; Function: Same asSCAN2 including BEEPeffect.
; Input:SameasSCAN2
; Output:Same asSCAN2
; Regeffected:AFBCDEHLAF'BC'DE'HL'.
; Call:SCAN2BEEP.

SCAN:
        CALL    SCAN2
        CALL    BEEP
        RET

;**************************************************************
; Function: Scanthekeyboardanddisplay.Loopuntil
;  a key is detected.If the some keyisalready
;  pressedwhenthisroutinestartsexecution,
;  returnwhennextkeyisentered.
; Input:IXpointstothebuffercontainsdisplaypatterns.
;  20 digits require 40 bytes of data.(IX) contains
;  thepatternforthe leftmostdigit,(IX+39)contains        ---- page 13 ----
;  thepatternfortherightmostdigit.
; Output:internalcode ofthekeypressed.
; Destroyedreg.:AF,BC,HL,AF',BC',DE',HL'.
;  All other registers except IY are also
;  changed during execution, but they are
;  restoredbeforereturn.
; Call: SCAN1

SCAN2:
        PUSH    IX
        LD      HL,TEST
        BIT     7,(HL)  ;This bit is set if user
                        ;has entered illegal key. The
                        ;display will be disabled as
                        ;a warning to the user. This
                        ;is done by replacing the display
                        ;buffer pointer IX by BLANK.
        JR      Z,SCPRE
        LD      IX,BLANK

; Waituntilallkeysarereleasedfor47ms.
; (The executiontime of SCANlis 15.7 ms,
; 47=15.7*3).

SCPRE:  LD      B,3
SCNX:   CALL    SCAN1           ;Get position code.
        JR      NC,SCPRE        ;If any key is pressed,
                                ;reload the debounce counter
                                ;B by 3.
        DJNZ    SCNX
        RES     7,(HL)          ; Clear error flag.
        POP     IX              ;Restore original IX.
                                
; Loop until any key is pressed.

SCLOOP: CALL    SCAN1
        JR      C,SCLOOP

; Convertthekey-position-code returnedbySCAN1 to
; ASC II code.Thisisdoneby table-lookup.
; ThetableusedisKEYTAB.

KEYMAP:
        LD      HL,KEYTAB
        LD      C,A
        LD      B,0
        ADD     HL,BC
        LD      A,(HL)
        PUSH    AF
        IN      A,(DIG3)
        BIT     4,A
        JR      Z,KSHIFT        ;SHIFT KEY?
        BIT     5,A
        JR      Z,KCTRL         ;CONTROL KEY?
        POP     AF
        RET

;Executedbydepressinganykeywith CTRLkey together.      ---- page 14 ----
;The key code is one byte stored in A register.

KCTRL:
        POP     AF
        RES     6,A
        RET

;ExecutedbydepressinganykeywithSHIFTkeytogether.
;The key code is one byte stored in A register.

KSHIFT:
        POP     AF
        SUB     2CH
        JR      C,SCLOOP
        CP      25H
        JR      NC,SCLOOP
        LD      HL,SHIFTT
        LD      C,A
        LD      B,0
        ADD     HL,BC
        LD      A,(HL)
        CP      0FFH
        JR      Z,SCLOOP        ;Zero,if illegal key in.
        RET
;
;**************************************************************
; Function;Scankeyboardanddisplayonecycle.
;   Total executiontimeisabout16ms(exactly
;   15.7ms,28040clockstates@1.79MHz).Input;SameasSCAN.
; Output: i)nokeyduringonescan
;           Carryflay--1
;        ii) key pressed during one scan
;             Carryflag--0,
;             A -- positioncodeofthekeypressed.
;                  If more than one key is pressed, A
;                  containsthe largestposition-code.
;                  (Thiskeyisthelastkeyscannd.)
; Destroyed reg: AF, AF', BC', DE', HL'. (see comments on SCAN)
; Call:none.


SCAN1:
;In hardware, the display and keyboard are
;arranged as a 20 by 3 matrix. Each cloumn
;corresponds to one digit and three key buttons.
;In normal operation, at most one column is
;active. The pattern of the active column is the
;data output on the port A,B,C of 8255 I. The data input
;from bit 0-2~5 on port C of 8255 II are the status of key
;buttons in the active column. All signals on
;I/O port are active low.

        SCF
        EX      AF,AF'
        EXX
        
;Carry flag of F' is used to return the status of
;the keyboard. If any key is pressed during one        ---- page 15 ----
;scan,theflagisreset;otherwise,itisset.
;Initially, thisflagisset.A'registerisused
;tostoretheposition-codeofthekeypressed.
;Inthisroutine,60keypositionsarecheckedone
;byone.Cregistercontainsthecodeofthekey
;beingchecked.Thevalue of Cis0atthebeginning,
;andisincreasedby1aftereachcheck.Sothecode
;rangesfrom0to3BH(total60positions).Oneach
;check,iftheinputbitis0(keypressed),Cregister
;iscopiedintoA'.ThecarryflagofF'issetalso.
;Whensomekeyisdetected,thekeypositionsafter
;thiskeywillstillbechecked.Soifmorethan
;onekeyarepressedduringone scan,thecodeofthe
;lastonewillbereturned.
        LD      C,0
        LD      DE,0FFFEH       ;Activatethefirst digit.
        LD      L,D
        LD      H,20            ;20digits.
KCOL:   LD      A,(IX+0)
        OUT     (SEG1),A        ;Firstbytepattern.
        INC     IX
        LD      A,(IX+0)
        OUT     (SEG2),A        ;2ndbytepattern.
        LD      A,E
        OUT     (DIG1),A        ;1-8digits
        LD      A,D
        OUT     (DIG2),A        ;9-16 digits
        LD      A,L
        OUT     (DIG3),A        ;17-20digits
        LD      B,COLDEL
        DJNZ    $               ;Delay1.5msperdigit.
        PUSH    DE
        LD      B,3             ;Eachcolomnhasthreekeys.
        IN      A,(KIN)         ;Now,bit02ofAcontainsthe
                                ;statusofthethreekeys in
                                ;theactivaecolomn.
        LD      D,A
KROW:   RR      D               ;RotateD1 bit right
                                ;bit0of D willberotate
                                ;intocarry flag.
        JR      C,NOKEY         ;Skipnext2instruction
                                ;ifthekeyisnotpressed.
                                ;Thenext 2instructions
                                ;storethecurrentposition-code
                                ;intoA'andresetcarryflag
                                ;ofF'register.
        LD      A,C
        EX      AF,AF'
NOKEY:  INC     C               ;Increasecurrentkeycodeby1.
        DJNZ    KROW            ;Loopuntil3keysinthe active
                                ;colomnsareallchecked.
        LD      A,0FFH          ;Disableallthe digits.
        OUT     (DIG1),A
        OUT     (DIG2),A
        OUT     (DIG3),A
        INC     IX
        POP     DE
        AND     A               ;                      ---- page 16 ----
        RLC     E
        JR      C,RL1
        SET     0,E
RL1:    RL      D
        JR      C,RL2
        SET     0,D
RL2:    RL      L
        DEC     H
        JR      NZ,KCOL
        LD      DE,-40
        ADD     IX,DE           ;GetoriginalIX.
        EXX
        EX      AF,AF'
        RET

;**************************************************************

;Exeutedwhen'M'keyispressed.
;  Enterthehexadecimaladdressofthefirstofthe
;fourmemorylocationstobedisplayed.
; (1) Type<CR>	Displayspecifiedmemorycontents.
; (2) Type	Altermemorycontents.
; (3) Type	Memorydump.
; (4) Type/	Movedatablockfromoneareatoanother.

MEMEXC:
        CALL    MEMEX2
        LD      A,10H
        LD      (TYPEFG),A      ;Setmemorytype.
        LD      A,(DE)
        CP      3AH             ;:
        JP      Z,MMODFY
        CP      2EH             ;.
        JR      Z,MDUMP1
        CP      2FH             ;/
        JR      Z,MMOVE
        CALL    MEMEX3          ;Displayspecifiedmemory
                                ;contents.
        CALL    HEXX
P102:   CALL    MEM3
        CALL    CR2
        RET

;**************************************************************

;Theinputdataustbehexadecimalvalues.MPF_iPwill
;ignorethiscommandifthereexistsatleastonedigit
;whichisnotahexadecimalvalue.Theusercanusethe
;BACKSPACEkey tocorrectthedata.

MEMEX2:
        CALL    ECHO_CH          ;Echothe inputcharacterand
                                ;prompt.
MEMEX1: CALL    GET             ;Getastringofcharacters
                                ;andendtheinputwith <CR>.
        CALL    CHKINP          ;Checkhexadecimalvalues.
        JR      C,MEMEX1        ;JumptoMEMEXliftheinput
                                ;data is illegal.      ---- page 17 ----
        CALL    CHKHEX          ;Get the hexadecimal address
                                ;of the first of four memory
                                ;locationstobedisplayed,
        LD      (MADDR),HL
        RET
        

MEMEX3:                         ;Reset the counters of INPUT
                                ; BUFFER and DISPBF BUFFER.
        PUSH    HL
        LD      HL,INPBF+4
        LD      (OUTPTR),HL
        LD      HL,DISPBF+8
        LD      (DISP),HL
        POP     HL
        RET
        
;**************************************************************

;If you want to change the value in these location,
;just type a colon and the values separated by spaces.
;The final command look like this:
;<M>=<start>:<data1 <data2> <data3><CR>

MMODFY:
SET:
        PUSH    HL
        CALL    GETHL           ;Get data.
        POP     HL
        LD      (HL),A
        JP      Z,CR3
        INC     HL
        JR      SET

;**************************************************************

;Type the first address,followed by a period and a second
;address .This two-address-separated-by-a-period form is
;called a memory range.
;If PRT_MPF exists ,then it will print out the data,
;otherwise MPF_IP will ignore this command
;The final fonnamd look like this:
;<M>=<start>.<end><CR>   or
;<M>=<start>.<end> <linking address><CR>

MDUMP1:
        CALL    PTEST
        RET     NZ
        LD      A,30H           ;Set memory dump type.
        LD      (TEST5),A
        CALL    MDUMP
        RET
        
;**************************************************************

;You can treat a range of memory (specified by two address
;separated by a slash),move it from one place to another
;in memory by using the MOVE command.                  ---- page 18 ---- 
;The final command look like this:
;<M>=<start>:<end> <destination><CR>

MMOVE:
        LD      (STEPBF),HL     ;The starting address in HL.
        CALL    GETHL           ;Get the ending address.
        LD      (STEPBF+2),HL
        CALL    GETHL           ;Get the destination address.
        LD      (STEPBF+6),HL
        CALL    GMV
        JP      CR3

;**************************************************************

GMV:
        LD      HL,STEPBF
        CALL    GETP            ;Load parameters from
                                ;step buffer into registers.
                                ;Also check if the parameters
                                ;are legal. After GETP,
                                ;HL = start address of source
                                ;BC = lenght to MOVE.
        JP      C,ERROR         ;Jump to ERROR if the
                                ;parameters are illegal.(i.e.,
                                ;ending address < startting address.)
        LD      DE,(STEPBF+4)   ;Load destination
                                ;address into DE.
        SBC     HL,DE           ;Compare HL and BC to
                                ;determine to move up or down.
        JR      NC,MVUP
                                ;Move down
        EX      DE,HL           ;HL = destination address.
        ADD     HL,BC           ;HL = dest.address+length
        DEC     HL              ;HL = end address of dest.
        EX      DE,HL           ;DE = end address of dest.
        LD      HL,(STEPBF+2)   ;HL = end address of source.
        LDDR                    ;Block transfel instruction.
        INC     DE              ;DE = last address moved.
        RET
MVUP:                           ;Move up
        ADD     HL,DE           ;HL is changed by
                                ;SBC HL,DE. Restore HL.
        LDIR                    ;Block transfer
        DEC     DE              ;DE = last address moved.
        RET
        
;**************************************************************

;To display four consecutive memory contents.

MEM3:
        LD      HL,(MADDR)
        LD      B,4
MEM5:
        CALL    SPACE1          ;Insert a space.
        LD      A,(HL)
        CALL    HEX2
        INC     HL              ;                      ---- page 19 ----
        DJNZ    MEM5
DECDSP:                         ;Clear the rightmost display
                                ;pattern.
                                ;in DISLPAY buffer,The display
                                ;pattern is usually a cursor.
        LD      IX,DISPBF
DEC_SP: LD      A,0FFH
        LD      HL,(DISP)
        LD      (HL),A
        INC     HL
        LD      (HL),A
        RET
        
;**************************************************************

;Executed when UP or down arrow key is pressed.
;Executed in memory mode only.

MFOR:                           ;Display next four memory
                                ;contents.
        LD      HL,(MADDR)
        INC     HL
        INC     HL
        INC     HL
        INC     HL
P101:   LD      (MADDR),HL
        LD      A,'M'
        CALL    ECHO_CH         ;Getpattern'<M>='
        CALL    HEXX
        JP      P102
MBACK:                          ;Display last four memory
                                ;contents.
        LD      HL,(MADDR)
        DEC     HL
        DEC     HL
        DEC     HL
        DEC     HL
        JR      P101

;**************************************************************

;Executed when 'F' key is pressed.
;Store the data byte into all memory locations from
;add1 to addr2.
;The final command look like this:
;<F>=<addrl> <addr2> <data><CR>

FILLDA:
        CALL    MEMEX2          ;Get starting address.
        CALL    RAMCHK
        JP      NZ,ERROR        ;Jump to ERROR if the
                                ;memory location of the
                                ;starting address is not RAM.
        PUSH    HL
        CALL    GETHL           ;Get endingaddress.
        PUSH    HL
        CALL    GETHL           ;Getdata.
        LD      A,L             ;                   ---- page 20 ----
        AND     A
        POP     HL
        POP     DE
        LD      (DE),A
        SBC     HL,DE
        RET     Z
        JP      C,ERROR         ;Jump to ERROR if starting
                                ;address > ending address.
        LD      B,H
        LD      C,L
        LD      H,D
        LD      L,E
        INC     DE
        LDIR
        JP      CR3

;**************************************************************

;Exeuted when 'I' key is pressed.
;MPF_IP will display the current limit address.
;SYSTEM RAM data that of course cannot be shifted
;so we must set the limit address of INSERT command.
;When one byte is inserted at some address,all
;data below this address will be shifted down one
;position.
;The last location will be shifted out and therefore lost.

; (1) Type <CR> -- To see the current limit address
;                       of INSERT command.
; (2) Type C    -- To clear limit address (i.e.,set
;                       limit address to be 0FE00H).
; (3) Enter the hexadecimal address -- To set new high
;                       limit address of INSERT command.

;When MPF_IP display <I>=
; You can enter the hexadecimal address and values separated
;                       by spaces -- To insert a block of data.
;The final command look like this:
;<!>=<address> <datal> <data2> <data3> <data4><CR>


INSET:
        CALL    INSET4
INSET3:
        LD      (STEPBF),HL
        INC     HL
        LD      (STEPBF+4),HL
        LD      DE,(END_ADDR)
        DEC     DE
        LD      (STEPBF+2),DE
        INC     DE
        AND     A
        SBC     HL,DE
        JP      NC,ERROR
        CALL    GETHL
        PUSH    AF

;Routine GMV needs 3 parameters which are stored in    ---- page 21 ----
;step-buffer (STEPBF):
;STEPBF  : starting address (2bytes).
;STEPBF+2: ending address (2bytes).
;STEPBF+4:destination address (2bytes).

        CALL    GMV
        LD      HL,(STEPBF+4)
        LD      (HL),A
        LD      HL,(STEPBF)
        INC     HL
        POP     AF
        JP      Z,CR3
        JR      INSET3
INSET4:
        PUSH    AF
        CALL    ECHO_CH        ;Echo the input character with
                                ;<?>=
                                ;? is I or D according to INSERT.
                                ;command or DELETE command
                                ;respectively.
        LD      HL,(END_ADDR)   ;Get the current limit address.
        CALL    HEXX
        LD      A,2FH           ;/
        CALL    CHRWR    ; '<I>=< current limit address>/^
        SCF
        CCF
INSET1:
        CALL    GET             ; Get a string of characters
                                ; end the input with <CR>.
        LD      A,(INPBF+9)
        JR      Z,INSET2        ;For <CR> condition.
        CP      'C'
        CALL    Z,CLRI          ;For C condition.
        JR      Z,INSET2
        LD      HL,INPBF+4
        CALL    CHKHE2          ;Get new limit address.
        JR      C,INSET1        ;Jump to INSET1 if the input
                                ;datas is illegal.
        LD      (END_ADDR),HL
INSET2:
        CALL    CR3              ;Print message.
        POP     AF
        CALL    MEMEX2
        RET

;*************************************************************

; Executed when 'n' key is pressed.
; MPF_IP will display the current limit address of DELETE
; command.
; Avoid to changing the contents in SYSTEM RAM we must
; set thelimit address.
; The default value of limit address is 0FE00H.

; (1) Type <CR> -- To see the current limit address.
; (2) Type C    -- To clear limit address (i.e.,set the
;                     limit address to be 0FE00H).
; (3) Enter the hexadecimal address -- To set the new  ---- page 22 ----
;                                      limitaddress.

; When MPF IP display <D>= you can enter the hexadecimal
; address which content you want to be deleted.
; You can delete one byte from memory at a time.

DELETE:
        CALL    INSET4
        LD      (STEPBF+4),HL
        LD      DE,(END_ADDR)
        LD      (STEPBF+2),DE
        INC     HL
        LD      (STEPBF),HL
        CALL    GMV
        XOR     A
        LD      (DE),A
        JP      CR3

;**************************************************************

; Executed when 'J' key is pressed.
; Instruction JR and DJNZ requires relative addresses.
; MPF_IP supports the calculation of relative addresses
; throught the 'J' command.

JUMP:
        CALL    MEMEX2      ;Get starting address.
        PUSH    HL
        CALL    GETHL       ;Get destination address.
        POP     DE          ;Load starting address
                            ;into DE.
        INC     DE          ;Increase this address by 2.
                            ;Relative address is used in
                            ;instruction JR and DJNZ.
                            ;The codes for them are 2 bytes.
                            ;The PC is increased by 2 after
                            ;opcode is fetched.
        INC     DE
        OR      A
        SBC     HL,DE       ;Load destination 
                            ;address into HL.
        LD      A,L         ;Check if the offset is between
                            ;+127 (007FH) and -128 (FF80H).
                            ;If the offset is positive, both
                            ;H and bit 7 of L must be zero;
                            ;if it is negative, H and but 7 of
                            ;L must be FF and 1. In both cases
                            ;adding H with bit 7 of L results
                            ;in 0.
                            ;Rotote bit 7 of L into carry
                            ;flag.
        RLA
        LD      A,H
        ADC     A,0         ;Add H and bit 7 of L.
        JP      NZ,ERROR    ;Branch to ERROR if
                            ;the results is nonzero.
        LD      A,L
        DEC     DE          ;                          ---- page 23 ----
        LD      (DE),A      ;Save the offset into
                            ;the next byte of opcode.
                            ;(JR or DJNZ)
        JP      CR3
        
;**************************************************************

;Executed when 'B' ket is pressed.
;The MPF_IP will display the current address of breakpoint.
; (1) Type <CR> -- To see the currently address of breakpoint
;                          address.
; (2) Type C    -- To clear breakpoint.
; (3) Enter the hedadecimal address -- To set new breakpoint.

BREAK:
        CALL    ECHO_CH ;Echo the input character with <B>=
DISBR:  LD      HL,(BRAD)
        CALL    HEXX    ;Display the current assigned breakpoint.
        LD      A,2FH   ;/
        CALL    CHRWR   ; <B>=< current break point address >/^
        SCF
        CCF
BREAK1:
        CALL    GET
        LD      A,(INPBF+9)
        JR      Z,B3            ;For <CR> condition.
        CP      'C'             ; For C condition.
        CALL    CLRB
        JR      Z,B3
        LD      HL,INPBF+4
        CALL    CHKHE2          ;Get new breakpoint address
                                ;stored into HL.
        JR      C,BREAK1        ;Jump to BREAK1 if the datas
                                ;are not hexadecimal values.
        LD      (BRAD),HL
B3:
        LD      HL,(BRAD)
        LD      A,(HL)
        LD      (BRDA),A
        JP      CR3

;**************************************************************

;Executed when 'S' is pressed.
;Execution at specified address or current address.

STEP:
        LD      B,A
        LD      A,(STEPFG)
        AND     A
        LD      A,B
        JR      NZ,P111         ;If zero,then execute at the
                                ;current address.
        CALL    MEMEX2          ;Get the specified address.
        LD      (USERPC),HL
P111:   LD      A,11101111B     ;This data will be output to
                                ;port C of 8255 II to enable BREAK.
                                ;It is done by routine PREOUT. ---- page 24 ----
        JR      PREOUT

;
;**************************************************************

;Executed when 'G' key is pressed.
;Execution at specified address or current address.
;The following routine is the service routine for
;'GO' key.

GOEXEC:
        CALL    ECHO_CH         ;Echo the input character with <G>=
GOEXE1: CALL    GET             ;Get a string of characters
                                ;end the input with <CR> .
        LD      A,(INPBF+4)
        CP      0DH
        JR      Z,EXEC2         ;If zero ,then execute at
                                ;the currentaddress.
        CALL    CHKHEX          ;Get specified address.
        JR      C,GOEXE1        ;Jump to GOEXE1 if one of
                                ;the input datum is illegal.
        LD      (USERPC),HL
EXEC2:
        LD      HL,(BRAD)       ;Get the address of breakpoint.
        LD      (HL),0EFH       ;Instruction RST28H.
                                ;The content of breakaddress
                                ;is changed to RST28H before
                                ;the control is transfered to
                                ;user's program. This
                                ;will cause a trap when user's
                                ;PC passes this point.
        LD      A,0FFH          ;Save FF into TEMP1. This data
                                ;will be output to port Clater.
                                ;FF isused to disable breakpoint.
PREOUT: LD      (TEMP1),A
        LD      A,0A5H
        LD      (STEPFG),A
        LD      A,(USERIF)      ;Save two instructions into
                                ;TEMP and TEMP+1. These two
                                ;instructions will be executed
                                ;later.  If the user's IFF
                                ;(interrupt flip-flop) is 1,
                                ;the instructions are 'EI RET'.
                                ;Otherwise, they are 'DI RET'.
        BIT     0,A
        LD      HL,0C9FBH       ;'EI','RET'
        JR      NZ,EIDI
        LD      L,0F3H          ;'DI'
EIDI:
        LD      (TEMP1+1),HL
        LD      SP,REGBF        ;Restore user's registers by
                                ;setting SP to REGBF (register
                                ;buffer) and continuously
                                ;popping the stack.
        POP     AF
        POP     BC
        POP     DE
        POP     HL              ;                      ---- page 25 ----
        EX      AF,AF'
        POP     AF
        EX      AF,AF'
        EXX
        POP     BC
        POP     DE
        POP     HL
        EXX
        POP     IX
        POP     IY
        LD      SP,(USERSP)         ;Restore user's SP.
        LD      (USERAF+1),A
        LD      A,(USERIF+1)        ;Restore user's I.
        LD      I,A
        PUSH    HL                  ;The next 3 instructions
                                    ;push the address being
                            ;displayed now (in USERPC)
                            ;onto stack without changing
                            ;HL register. This address will be
                            ;treated as user's new PC.
        LD      HL,(USERPC)
        EX      (SP),HL
        LD      A,(TEMP1)   ;Output the data stored in
                            ;TEMP1 to port C of 8255 II
                            ;This data is prepared by
                            ;command STEP or GO.
                            ;In first case, it is
                            ;11101111 and will enable
                            ;breakpoint. In other
                            ;cases, it is FF and will
                            ;disable breakpoint.
                            ;If break is enabled, non­
                            ;maskable interrupt will occur
                            ;5M1's after the OUT instruction.
        OUT     (KIN),A
        LD      A,(USERAF+1)    ;1st M1.
        JP      TEMP1+1 ;2nd M1,
                        ;Execute the two instructions
                        ;stored in RAM. They are:
                        ;   EI (or DI)      ;3rd M1
                        ;   RET             ;4th M1
                        ;The starting address of user's
                        ;programs has been pushed onto
                        ;the top of the stack. RET pops
                        ;out this address and transfers
                        ;control to it. The first M1
                        ;of the user's program will be the
                        ;5th M1 after OUT. If break point
                        ;is enabled, NMI will coour after
                        ;this instruction is completed.
                        ;This is the mechanism of single
                        ;step.

;**************************************************************

;The monitor reserves 26 locations in memory for the 
;twenty-one registers as follows:
; AF BC DE HL AF' BC' DE' HL' IX IY SP PC I            ---- page 26 ----

; Type <CR> -- Display registers (two pairs of registers).
; Type <register name><CR> -- Display registers (pair of
;                             registers)
; Type :    -- Alter register contents.

REGEXC:
        CALL    ECHO_CH ;Echo the input character with <R>=
REGEX2: CALL    GET             ;Get a string of characters
                                ;end the input with<CR>.
        LD      HL,INPBF+3
        PUSH    HL
        INC     HL
        CALL    CHKHE2
        POP     HL
        JR      C,REGEX2        ;Jump to REGEX2 if one
                                ;of the input datas is illegal.
        CALL    LDA
        LD      DE,0
        LD      C,D             ;Set C=0
        LD      A,0DH
        LD      D,(HL)
        CP      D
        JR      Z,REGALL        ;Display the first four register
                                ;contents(AF BC).
        INC     HL
        LD      A,(HL)
        CP      27H             ;Check ' condition.
                                ;(i.e.,A' F' B' C' D' E' H' L')
        JR      NZ,P105
        INC     HL
        LD      C,A
        LD      A,(HL)
P105:   CP      3AH             ;Check : condition
                                ;If zero ,then change the content
                                ;of a single byte register
                                ;(i.e.,AFBCDEHLA'F'B'C'D'E'H'L'I)
        JP      Z,RMODFY
        CP      0DH
        JR      Z,RDSPL3        ;If zero ,then display two single
                                ;byte register (A F A' F' I).
                                ;or a register pair (BC DE HL
                                ;BC' DE' HL').
                                ;or a two byte registers.
                                ;(IX IY SP PC)
        LD      E,(HL)
        INC     HL
        LD      A,(HL)
        CP      27H             ;Check ' condition.
                                ;(i•e., AF' BC' DE' HL')
        JR      NZ,P106
        INC     HL
        LD      C,A
        LD      A,(HL)
P106:   CP      3AH
        JP      Z,RMODF1        ;Change the contents of two
                                ;single byte register.
                                ;(AF AF' IF).          ---- page 27 ----
                                ;or a register pair (BC DE
                                ;HL BC' DE' HL').
                                ;or a two byte register (
                                ;IX IY SP PC).
        CP      0DH
        JR      Z,RDSPLY       ;Display two single byte
                                ;register or a register pair
                                ;or a two byte register.
                                
;**************************************************************

REGALL:                         ;Display 'AF BC'
        LD      DE,4100H

RDSPL4:                         ;Display four bytes of register
                                ;contents.
        LD      B,2
        JR      RDSPL0
RDSPL3:
        LD      A,D
        CP      'I'             ;Chgeck I register.
        JR      NZ,RDSPLY
        LD      E,46H
        
;**************************************************************

;Display two single byte registers (AF AF' I) or
;a register pair ( BC DE HL BC' DE' HL' ) or
;a two byte register ( IX IY SP PC ).

RDSPLY:
        LD      B,1
RDSPL0:
        PUSH    BC
        CALL    SEARC_REG
        POP     BC
        JR      Z,REGALL        ;Jump to REGALL if the input
                                ;register name is illegal.
        CALL    MEMEX3
RDSPL6: LD      A,(RCOUNT)
        BIT     0,A
        JR      Z,RDSPL1
        DEC     HL
        DEC     DE
        LD      A,(RCOUNT)
        RES     0,A             ;Registers are displayed by
                                ;pair. Find the count of
                                ;pair leader. (count of
                                ;the lower one)
        LD      (RCOUNT),A
RDSPL1: CALL    SPACE1          ;Insert a space.
        LD      A,(HL)          ;Get the first register name.
        CALL    CHRWR
        INC     HL
        LD      A,(HL)          ;Get the second register name.
        CALL    CHRWR
        CALL    SPACE1          ;Insert a space.
        INC     DE              ;                      ---- page 28 ----
        LD      A,(DE)          ;Get the first register content.
        CALL    HEX2
        DEC     DE
        LD      A,(DE)          ;Get the second register content.
        CALL    HEX2
        INC     HL
        INC     DE
        INC     DE
        DJNZ    RDSPL1
        LD      A,20H           ;Set register mode.
        LD      (TYPEFG),A
        JP      REG2

;**************************************************************

;Executed when UP or DOWN arrow is pressed.
;Executed in register mode only.

RFOR:                           ;Display next four register contents.
        LD      A,(RCOUNT)
        INC     A
        INC     A
        INC     A
        INC     A
        CP      24
        JR      Z,RBACK1
        JR      NC,RBACK2
RFOR1:  LD      (RCOUNT),A
        LD      A,(STEPFG)      ;If the content of STEPBF is
                                ;zero.
                                ;it means MPF_IP executes STEP
                                ;or GO command.
        AND     A
        JR      Z,RFOR2
        CALL    CLEAR
        LD  HL,(USERPC)
        CALL    HEX4
        JR      RFOR3
RFOR2:  LD      A,52H           ;Get pattern '<R>='
        CALL    ECHO_CH
RFOR3:  LD      HL,RTABLE
        LD      A,(RCOUNT)
        ADD     A,L
        LD      L,A
        LD      D,(HL)
        INC     HL
        LD      E,(HL)
        JP      RDSPL4
RBACK:                          ;Display last four register
                                ;contents.
        LD      A,(RCOUNT)
        CP      2
        JR      Z,RBACK1
        JR      C,RFOR1
RBACK2: DEC     A
        DEC     A
RBACK1: DEC     A
        DEC     A               ;                      ---- page 29 ----
        JR      RFOR1
        
;**************************************************************

;There are four kinds of register modify mode as folloes:

;   (1) <R>=H:< one byte data ><CR>
;   (2) <R>=H':< one byte data ><CR>
;   (3) <R>=HL:< two byte data ><CR>
;   (4) <R>=HL':< two byte data ><CR>

RMODFY:
        CALL    SEARC_REG
        JR      Z,MEMDP3        ;Illegal register name.
        PUSH    DE
        CALL    GETHL
        POP     DE
        BIT     0,C
        JR      Z,RODD
        DEC     DE
RMODF2: LD      (DE),A
        JP      CR3
RODD:   INC     DE
        JR      RMODF2
RMODF1: CALL    SEARC_REG
        JR      Z,MEMDP3        ;Illegal register name.
        BIT     0,C
        JR      Z,RMODF3
        DEC     DE
RMODF3: PUSH    DE
        CALL    GETHL
        POP     DE
        LD      (DE),A
        INC     DE
        LD      A,H
        LD      (DE),A
        JP      CR3

;**************************************************************

; You can examine these registers when you STEP or GO
; a machine language.

MEMDP2:
        CALL    CLEAR
        LD      HL,(USERPC)
        CALL    HEX4
MEMDP3:
        JP      REGALL

;**************************************************************
; Find bases of the register name and contents.
; Input :Register name (ASC II code) stored in DE.
; Output: HL -- Base of RTABLE (i.e.,point to register
;                               name beginning).
;         DE -- Base of REGBF (i.e.,point to register
;                              buffer begining).
;          C -- Counts of register in RATBLE.          ---- page 30 ----

SEARC_REG:
        LD      HL,RTABLE
        PUSH    BC
        XOR     A
        LD      C,A
        LD      A,D
        LD      B,25
SERCH:  CP      (HL)        ;Compare with the first
                            ;register name.
        JR      NZ,SERCH1
        LD      A,E
        AND     A
        JR      Z,SERCH2    ;Zero,if it is a single
                            ;byte register.
        INC     HL
        INC     C
        CP      (HL)        ;Compare with the second
                            ;register name.

        JR      Z,SERCH2
        LD      A,D
        JR      SERCH4
SERCH2:
        LD      A,C
        POP     BC
        LD      B,A         
        LD      A,C
        CP      27H         ;Check ' (A' F' B' C' D' E' H' L')
        LD      C,B
        JR      NZ,SERCH3
        LD      A,L
        ADD     A,8
        LD      L,A
        LD      A,C
        ADD     A,8
        LD      C,A
SERCH3:
        LD      A,C
SERCH5: LD      (RCOUNT) ,A
        LD      DE,REGBF
        ADD     A,E
        LD      E,A
        RET
SERCH1: INC     HL
        INC     C
SERCH4: DJNZ    SERCH
        XOR     A
        AND     A
        POP     BC
        RET                 ;Zero if illegal register name exists.
;
;**************************************************************
;   Function: Refer to READLN.
;             C -- Get a string of characters.
;            NC -- Reset the content of INPTR.
;
GET:
        JP      C,RDLOOP      ;                        ---- page 31 ----
GETT:   CALL    READLN
        CALL    DECDSP
        RET
;
;**************************************************************
; TAPE WRITE:
DUMP:

        CALL    ECHO_CH         ;Echo the input character
                                ;with <W>=
DUMP1:  CALL    GET             ;Get a string of characters
                                ;and end the input with <CR>
        CALL    CHKHEX          ;Get starting address.
        JR      C,DUMP1         ;Jump to DUMP] if the input
                                ;datum are illegal.
        LD      (STEPBF+4) ,HL
        CALL    GETHL           ;Get ending address.
        JR      C,DUMP1         ;dump to DUMP1 if the input
                                ;datas are illegal.
        LD      (STEPBF+6) ,HL
DUMP2:  CALL    GETCHR          ;Get tape filename.
        LD      DE,STEPBF
        LD      BC,4
        LDIR
        CALL    SUM1            ;Load parameters from
                                ;step buffer into registers
                                ;Check if the parameters
                                ;are legal. If legal,calculate
                                ;the sum of all data to be
                                ;output to tape.
        JR      C,ERROR         ;Branch to ERROR if the
                                ;Parameters are illegal. (length
                                ;is negative)
        LD      (STEPBF+8) ,A   ;Store the ckecksum into
                                ;STEPBF+8.
        LD      HL,4000         ;Output 1K Hz square
                                ;wave for 490% cycles.
                                ;Leading sync signal.
        CALL    TONE1K
        LD      HL,STEPBF       ;Output 27 bytes starting
                                ;at STEPBF. (Include:
                                ;filename ,Sstarting,ending
                                ;address and checksum and all
                                ;the parameters of EDITOR and
                                ;ASSEMBLER.
        LD      BC,27
        CALL    TAPEOUT
        LD      HL, 4000        ;Output 2K Hz square
                                ;waves for 4000 cycles.
                                ;Middle sync. The filename of
                                ;the file being read will be
                                ;displayed in the interval.
        CALL    TONE2K
        CALL    GETPTR          ;Load parameters into
                                ;registers(Starting,ending and
                                ;length).
        CALL    TAPEOUT         ;Output user's data
        LD      HL, 4000        ;Output 4000 cycles    ---- page 32 ----
                                ;of 2K Hz square wave.
                                ; (Tail sync.)
        CALL    TONE2K
        RET
;
;**************************************************************
; Function: Print ERROR message.
; Input: None
; Output: Display patterns ' ERRORS' in display buffer.
; (OUTPTR) <~ INPBF+8
; (DISP) <- DISPBF+16
; Reg affected: AF HL .
; Call: PRTMES

ERROR:
        LD      HL, ERRSMSG
        JP      PRTMES
;
;**************************************************************
; Function: TAPE READ .

LOAD:
        CALL    ECHO_CH         ;Echo the input character
                                ;with <L>=
        CALL    GET             ;Get a string of characters
                                ;and end the input with <CR>.
LEAD:   LD      A,10111111B     ;Decimal point.
        OUT     (SEG2) ,A       ;When secrching for filename
                                ;the display is blank intially.
                                ;If the data read from MIC is
                                ;acceptable 0 or 1,the display
                                ;becomes ' 
        LD      A, 0FFH
        OUT     (SEG1),A
        LD      HL,1000
LEAD1:  CALL    PERIOD          ;The return of PERIOD
                                ;is in flag:
                                ; NC -- tape input is 1K Hz
                                ; C  -- otherwise
        JR      C, LEAD         ;Load until leading sync.
                                ;is detected.
        DEC     HL              ;Decrease HL by one when
                                ;one period is detected.
        LD      A,H
        OR      L               ;Check if both H and L are zero.
        JR      NZ,LEAD1        ;Wait for 19606 periods.
                                ;The leading sync is accepted
                                ;if it is longer than 1000
                                ;cycles (1 second).
LEAD2:  CALL    PERIOD
        JR      NC, LEAD2       ;Wait all leading sync to
                                ;Pass over.
        LD      HL, STEPBF      ;Load 27 bytes from
                                ;tape into STEPBF.
        LD      BC,27
        CALL    TAPEIN
        JR      C,LEAD          ;Jump to LEAD if input
                                ;is not sucessful.    ----- page 33 ----
        LD      B,4             ;Get filename from DISPLAY BUFFER.
                                ;The filename is consisted of 4
                                ;alphanumeric characters.
        LD      HL,DISPBF+18
        LD      (DISP) ,HL
        LD      HL,STEPBF
LOOP3:  LD      A, (HL)
        CALL    CONVER
        INC     HL
        DJNZ    LOOP3
        LD      B,196           ;Display it for 1.57 sec.
FILEDP: CALL    SCAN1
        DJNZ    FILEDP
        LD      B,4             ;Check if the input
                                ;filename equals to the
                                ;specified filenames.
        DEC     HL
        LD      DE, (OUTPTR)
        DEC     DE
LOOP4:  LD      A, (DE)
        CP      (HL)
        DEC     HL
        DEC     DE
        JR      NZ,LEAD         ;If not,find the leading
                                ;Sync of next filename.
        DJNZ    LOOP4
        LD      A,3FH           ;If filename is found
                                ;then display '
        OUT     (SEG1),A
        LD      A, 0FFH
        OUT     (SEG2) ,A
        CALL    GETPTR          ;The parameters (starting
                                ;ending address and checksum)
                                ;have been load into STEPBF.
                                ;Load them into registers,
                                ;Calculate the block length
                                ;and check if they are legal.
        JR      C,ERROR         ;Jump to ERROR if input
                                ;is not successful.
        CALL    TAPEIN          ;Input user's data.
        JR      C,ERROR
        CALL    SUM1            ;Calculate the sum of all
        LD      HL, STEPBF+8
        CP      (HL)            ;Compare it with the
                                ;checksum calculated by and
                                ;Stored by 'W' FUNCTION.
        JP      NZ,ERROR        ;Jump to ERROR if not
                                ;matched,
        RET

;**************************************************************
TAPEIN:
; Load a memory block from tape.
; Input: HL -- starting address of the block
;       BC -- length of the block
; Output: Carry flag,1 -- reading error
;                    0 -- no error
; Destroyed reg. -- AF,BC,DE,HL,AF',BC',DE',HL'        ---- page 34 ----
        XOR     A               ;Clear carry flag.
        EX      AF,AF'
TLOOP:  CALL    GETBYTE         ;Get one byte from TAPE.
        LD      (HL) ,E         ;Store it into memory.
        CPI
        JP      PE,TLOOP        ;Loop until length is zero.
        EX      AF,AF'
        RET

GETBYTE:
; Read one byte from tape.
; Output: E -- data read
;         Carry of F',1 -- reading error
;                     0 -- no error
; Destroy reg. -- AF,DE,AF',BC',DE',HL'
; Byte format:

; start bit bit bit bit bit bit bit bit stop
;  bit   0   1   2   3   4   5   6   7   bit

        CALL    GETBIT          ;Get start bit.
        LD      D,8             ;Loop 8 times.
BLOOP:  CALL    GETBIT          ;Get one data bit
                                ;result in carry flag.
        RR      E               ;Rotate it into E .
        DEC     D
        JR      NZ,BLOOP
        CALL    GETBIT          ;Get stop bit .
        RET
GETBIT:
; Read one bit from tape.
; Output: Carry of F,0 -- this bit is 0
;                    1 -- this bit is 1
;        Carry of F',1 -- reading error
;                    0 -- no error
; Destroyed reg. -- AF,AF',BC',DE',HL'
; Bit format:

; @ -- 2K Hz 8 cycles + 1K Hz 2 cycles.
; 1 -- 2K Hz 4 cycles + 1K Hz 4 cycles.
        EXX

; The tape~bit format of both @ and 1 are
; of the same form: high freq part + low freq part.
; The difference between @ and 1 is the
; number high freq cycles and low freg
; cycles. Thus, a high freq period may has
; two meanings:
; ##i) It is used to count the number of high
; freq cycles of the current tape-bit;
; ii) If a high freq period is detected
; immediately after a low freq period, then
; this period is the first cycle of next
; tape-bit and is used as a terminator of the
; last tape-bit.

; Bit 0 of H register is used to indicate the usage
; of a high freq period. If this bit is zero, high     ---- page 35 ----
; freq period causes counter increment for the current
; tape-bit. If the high freq part has passed, bit @
; of H is set and the next high freq period will be used
; as a terminator.
; L register is used to up/down count the number of periods.
; when'a high freq period is read, L is increased by
; 1; when a low freq period is read, L is decreased
; by 2. (The time duration for each count is 8.5 ms.)
; At the end of a tape-bit, positive and negative L
; stand for 8 and 1 respectively.

        LD      HL, 8
COUNT1: CALL    PERIOD          ;Read one period.
        INC     D               ;The next two instructions
                                ;check if D is zero. Carry flag
                                ;is not affected.
        DEC     D
        JR      NZ, TERR        ;If D is not zero,jump
                                ;to error routine TERR .
                                ; (Because the period is too
                                ;much longer than that of 1K Hz).
        JR      C,SHORTP        ;If the period is short
                                ;(2K Hz),jump to SHORTP.
        DEC     L               ;The period is 1K Hz,
                                ;decrease L by 2 . And set
                                ;bit @ of H to indicate this
                                ;tape-bit has passed high freq
                                ;part and reaches its’ low freq
                                ;part.
        DEC     L
        SET     0,H
        JR      COUNT1
SHORTP: INC     L               ;The period is 2K Hz ,
                                ;increase L by l.
        BIT     0,H             ;1f the tape bit has passed
                                ;its high freq part, high
                                ;frequency means this bit is all
                                ;over and next bit has started.
        JR      Z,COUNT1
                                ;L= (# of 2K period) ~ 2*(# of 1K period)
        RL      L 
                                ; @ -~~ NCarry (L positive)
                                ; 1 --~ Carry (L negative)
                                ;The positive or negative sign of
                                ;L. corresponds to the tape-bit data.
                                ;"RL L' will shift the sign bit of
                                ;L into carry flag. After this
                                ;instruction, the carry flag
                                ;contains the tape-bit.
        EXX                     ;Restore BC' DE’ HL‘
        RET
TERR:   EX      AF,AF'
        SCF                     ;Set carry flag of F' to indicate error.
        EX      AF,AF'
        EXX
        RET
PERIOD:
; Wait the tape to pass one period.                    ---- page 36 ----
; The time duration is stored in DE. The
; unit is loop count. We use 32 as the
; threshold for 2K Hz and 1K Hz.
; result is in carry flag. (1K -~ NC, 2K -~ C)
; Register AF and DE are destroyed.

        LD      DE, 00
LOOPH:  IN      A, (KIN)        ;Bit 3 of port C is Tapein.
        INC     DE
        BIT     3,A
        JR      Z,LOOPH         ;Loop until input goes low.
        LD      A,11011111B     ;Echo the tape input to
                                ;Speaker on MPF IP.
        OUT     (KIN) ,A
        LD      A, 0F0H
        OUT     (DIG2) ,A
        LD      A, 0FFH
        OUT     (DIG1),A
LOOPL:  IN      A, (KIN)
        INC     DE
        BIT     3,A
        JR      NZ ,LOOPL ;Loop until input goes high.
        LD      A,11111111B     ;Echo the tape input to
                                ;Speaker on MPF_IP.
        OUT     (KIN) ,A
        LD      A,E
        CP      MPERIOD         ;Compare the result with
                                ;the threshold,
        RET
;
;**************************************************************
SUM1:
; Calculate the sum of the data in a memory
; block. The starting and ending address
; of this block are stored in STEPBF+2 ~ STEPBF+4,
;   Registers AF,BC,DE,HL are destroyed.

        CALL GETPTR             ;Get parameters from
                                ;step buffer.
        RET     C               ;Return if the parameters
                                ;are illegal.
SUM:
; Calculate the sum of a memory block.
; HL contains the starting address of
; this block, BC contains the length.
; The result is stored in A. Registers
; AF,BC,HL are destroyed.

        XOR     A               ;Clear A
SUMCAL: ADD     A, (HL)
        CPI
        JP      PE,SUMCAL
        OR      A               ;Clear flag.
        RET
GETPTR:
; Get parameters from step buffer.
; Input: (STEPBF+4) and (STEPBF+5) contain
;        starting address.                             ---- page 37 ----
;        (STEPBF+6) and (STEPBF+7) contain
;        ending address.
; Output: HL register contains the starting
;        address,
;         BC register contains the length.
;         Carry flay 0 -- BC positive
;                    1 -- BC negative
; Destroyed reg.: AF,BC,DE,HL.

        LD      HL,STEPBF+4
GETP:   LD E, (HL)      ;Load the starting address
                                ;into DE .
        INC     HL
        LD      D, (HL)
        INC     HL
        LD      C, (HL)
        INC     HL
        LD      H, (HL)         ;Load ending address
                                ;into HL.
        LD      L,C
        OR      A               ;Clear carry flag.
        SBC     HL,DE           ;Find difference.
                                ;Carry flag is changed here.
        LD      C,L
        LD      B,H
        INC     BC              ;Now BC contains the
                                ;length.
        EX      DE,HL           ;Now HL contains the
                                ;Starting address.
        RET

;**************************************************************
TAPEOUT:
; Output a memory block to tape.
; Input: HL -- starting address of the block
;        BC -- length of the block
; Destroyed reg. -- AF,BC,DE,HL,BC',DE',HL'

        LD      E, (HL)         ;Get the data.
        CALL    OUTBYTE         ;Output to tape.
        CPI
        JP      PE, TAPEOUT     ;Loop until finished.
        RET
OUTBYTE:
; Output one byte to tape. For tape-byte
; format, see comments on GETBYTE.
; Input: E ~- data
; Destroyed reg. ~~ AF,DE,BC',DE',HL'

        LD      D,8             ;Loop 8 times.
        OR      A               ;Clear carry flag.
        CALL    OUTBIT          ;Output start bit.
OLOOP:  RR      E               ;Rotate data into carry.
        CALL    OUTBIT          ;Output the carry.
        DEC     D
        JR      NZ,OLOOP
        SCF                     ;Set carry flag.
        CALL OUTBIT             ;Output stop bit.      ---- page 38 ----
        RET
OUTBIT:
; Output one bit to tape.
; Input: data in carry flag.
; Destroyed reg. -~ AF,BC',DE',HL'
        EXX
        LD      H,0
        JR      C,OUT1          ;If data = 1 ,outputl.
OUT1:   LD      L,ZERO_2K
        CALL    TONE2K
        LD      L,ZERO_1K
        JR      BITEND
OUTIL:                          ;2K 4 cycles ,1K 4 cycles
        LD      L,ONE_2K
        CALL    TONE2K
        LD      L,ONE_1K 
BITEND: CALL    TONE1K
        EXX                     ;Restore registers.
        RET
;
;**************************************************************
; Function: Clear display buffer and display prompt.
; Input: None
; Output: (OUTPTR) <- INPBF
;         (DISP)   <- DISPBF
;               IX <- DISPBF
;         Set all the contents of display buffer to be FF .
; Reg affected: AF IX.
; Call: CLEAR CHRWR . 

CLRBF:
        CALL    CLEAR
        LD      A,3CH 
        CALL    CHRWR
        LD      IX,DISPBF
        RET
;
;**************************************************************
; Function: Generate a sound.
; Input:None
; Output: None
; Reg affected: AF BC DE HL. ,
; Call: TONE

BEEP: PUSH AF
        LD      HL, BEEPSET
        LD      A, (HL)
        AND     A
        JR      NZ ,NOTONE
        INC     HL
        LD      C, (HL)
        LD      HL, (TBEEP)
        CALL    TONE
        OR      20H             ;LED off.
        OUT     (KIN) ,A
NOTONE:
        POP     AF
        RET                     ;                      ---- page 39 ----
;
;**************************************************************
; Function: check if a memory address is in RAM.
; Input: HL -~ address to be check.
; Output: Zero flag -~ 8, ROM or nonexistant;
; 1, RAM.
; Destroyed reg.: AF.
; Call: none

RAMCHK:
        LD      A, (HL)
        CPL
        LD      (HL) ,A
        LD      A, (HL)
        CPL
        LD      (HL) ,A
        CP      (HL)
        RET
;
;**************************************************************
; Function: Convert a byte (ASC II code) in A register
;           to display pattern.
; Input: A -- ASC II code.
;       (DISP) -- Point to the result address in display buffer.
; Output: Pattern for two bytes. The first byte in (DISP) and
;         the second byte in (DISP)+1 .
;         (DISP) <- (DISP)+2
; Reg affected: AF
; Call: None

CONVER:
        PUSH    BC
        PUSH    DE
        PUSH    HL
        LD      HL, SEGTAB
        LD      B,20H
        SUB     B
        ADD     A,A
        LD      C,A
        LD      B,0
        ADD     HL,BC
        LD      E, (HL)
        INC     HL
        LD      D, (HL)
        LD      HL, (DISP)
        LD      (HL) ,E
        INC     HL
        LD      (HL) ,D
        INC     HL
        LD      (DISP),HL
        POP     HL
        POP     DE
        POP     BC
        RET
;
;**************************************************************
; Function: Clear the display buffer.
; Input: None                                          ---- page 40 ----
; Output: Set all contents in display buffer to be FF.
; Reg effected: None
; Call: None

CLRDSP:
        PUSH    HL
        PUSH    DE
        PUSH    BC
        LD      HL, DISPBF
        LD      DE,DISPBF+1
        LD      BC, 80
        LD      (HL) ,0FFH
        LDIR
        POP     BC
        POP     DE
        POP     HL
        RET
CHKINP:                         ;Check alli the datum in input
                                ;buffer are hexadecimal values onitorIP.or not
                                ;until <CR> met.
                                ;Carry flag is set if there exists
                                ;at least one non hexadecimal value.
        CALL    CHKHEX
        RET     C
        RET     Z
CHKINI: CALL    GETHL
        RET     C
        RET     Z
        JR      CHKINI
ECHO_CH:                        ;Echo the input character
                                ;with <?>=
                                ;is the input character.
        CALL    CHRWR           ;?
        LD      A,3EH           ;>
        CALL    CHRWR
        LD      A,3DH           ;=
        CALL    CHRWR
        RET
;
;**************************************************************
; Function: Generate square wave to the MIC & speaker
;           on MPF_IP.
; Input :  C -- period = 2*(44+13*C) clock states,
;         HL -- number of periods.
; Output: none.
; Destroyed reg.: AF, B(C), DE, HL.
; Call: none.

TONE1K:
        LD      C,F1KHZ
        JR      TONE
TONE2K:
        LD      C,F2KHZ
TONE: 
        ADD     HL,HL
        LD      DE,1
        LD      A, 0FFH
SQWAVE: OUT     (KIN) ,A        ;                      ---- page 41 ----
        LD      B,C
        DJNZ    $
        XOR     20H             ; TOGGLE OUTPUT
        SBC     HL,DE
        JR      NZ ,SQWAVE
        RET
;
;**************************************************************
; Function: Print message until <CR> met.
; Input: HL -— Starting address of characters.
; Output: (OUTPTR) <~ (OUTPTR)+?
;         (DISP) <~ (DISP)+2*?
;         ? is the number of charcters to be printed.
;         2*? is fails if there exists TAB key in input buffer.
; Reg affected: AF HL.
; Call: CLEAR MSG DECDSP CR2 .

PRTMES:
        CALL    CLEAR
        CALL    MSG
REG2:   CALL    DECDSP
        CALL    CR2
        RET
;
;**************************************************************
; Function: Print out all the contents in display buffer.
; Input: None
; Output: None
; Reg affected: AF
; Call: PTEST MTPPRT .

PRINTT: CALL    PTEST
        RET     NZ
        PUSH    IX
        LD      IX, INPBF
        CALL    MTPPRT          ; Refer to printer manual.
        POP     IX
        RET
;
;**************************************************************
; Function: Check the toggle printer switch and
;           the condition of printer interface,
; Input: None
; Output: Zero flag = 1     (1) Printer exists and toggle
;                            switch is on.

;         Zero flag = 0     (2) Printer exists but the
;                            toggle switch is off.
;                           (3) Printer not exists.
; Reg affected: AF
; Call:None

PTEST:
        LD      A, (PRTFLG)
        AND     A
        RET     NZ
PTESTT:                         ; Check printer interface,
                                ; Carry flag = 1 if printer exists. ---- page 42 ----
        LD      A, (6988H)
        CP      0CDH
        RET
;
;**************************************************************
; Function: Use (GETPT) as a pointer increase HL until
;           (HL=1) is one of the following delimeters:
;           SPACE TAB .: / = and (HL+1) is not SPACE
;           or TAB.
; Input: HL = (GETPT) -~ Starting address.
; Output: HL <= HL+?
;         (GETPT) <- (GETPT) +?
; Reg affected: AF HL .
; Call: None

GETCHR:
        LD      HL, (GETPT)
LDA:
        LD      A, (HL)
        CP      ' '             ;SPACE..
        JR      Z,SKIP_
        CP      9H              ; TAB.
        JR      NZ ,EOS_Q
SKIP_:
        INC     HL
        LD      A, (HL)
        CP      ' '             ;SPACE.
        JR      Z,SKIP_
        CP      9H              ; TAB.
        JR      Z,SKIP_
STPTR: 
        LD      (GETPT) , HL
        RET
EOS_Q:
        CP      0DH             ;End of string?
        JR      Z,STPTR         ;Yes
        CP      3AH             ;:
        JR      Z,SKIP_
        CP      2EH             ;.
        JR      Z,SKIP_
        CP      3DH             ;=
        JR      Z,SKIP_
        CP      2FH             ;/
        JR      Z,SKIP_
        INC     HL
        JR      LDA
CHKHEX:
        LD HL, INPBF
CHKHE2:
        LD (GETPT),HL
;
;**************************************************************
; Function: Call GETCHR and convert ASC II codes to hexadecimal
; values and store them into HL .
; Input: (GETPT)
; Output: (GETPT) <- (GETPT)+?
; A <- G
; H=0 If there is only one hexadecimal digit.          ---- page 43 ----
;        Carry flag =1 If the data is not hexadecimal digits.
;        Zero flag = 1 If the last ASC II code is <CR> .
; Reg affected: AF DE HL .
; Call: GETCHR ONE .

GETHL:                          ;Get 4 digit number to HL &L=A
                                ;C (Non hexadecimal values)
                                ;Z (8DH)
        LD      HL,0            ;Assume input 0000
        PUSH    HL              ;Temporary store in (SP),(SP+1)
        ADD     HL, SP          ;HL=SP
        EX      DE,HL           ;Borrow SP for tempory buffer.
        CALL    GETCHR
        EX      DE,HL
CV3:    CP      '0'
        JR      NC,CVT
        CP      0DH
        JR      NZ,CV2
CV1:    POP     HL
        LD      A,L             ;String end.
        RET
CV2:
        AND     A
        JR      CV1
CVT:
        CP      3AH             ;:
        JR      Z,CV2
CVTHEX:
        CALL    ONE             ;ASCII to HEX
        JR      C,NOTHEX
        RLD                     ;Rotate into (HL) i.e. (SP)
        INC     HL              ;SP+1
        RLD
        DEC     HL
        INC     DE
        LD      A, (DE)
        JR      CV3
NOTHEX:                         ;Error
        POP     HL
        RET
;
;**************************************************************
; Function: Check the numbers of content in display buffer,
;           if it excess 40 the change the IX pointer.
; Input: (DISP)
; Output: IX <- IX        (If the number of contents are less
;                         than 40).
;         IX <- (DISP)-38 (If the numbers of contents are lager
;                         than 40).
;         Carry flag = 1  If (DISP) < (DISP)+38
; Reg affected: AF DE HL IX .
; Call: None

CHK46:
        LD      HL, (DISP)
        LD      DE, DISPBF+38
        AND     A
        SBC     HL,DE           ;                      ---- page 44 ----
        LD      IX, DISPBF
        RET     C
        EX      DE,HL
        ADD     IX,DE
        RET
;
;**************************************************************
; Function: Convert a byte (ASC II code) in A register
;           to display patterns and store them into
;           input buffer and display buffer respectively.
; Input: A -- a byte of ASC II code. ,
;        (OUTPTR) -- Point to the result address in input buffer.
;        (DISP) -- Point to the result address in display buffer.
; Output: Store the ASC II code into (OUTPTR)
;         Pattern for two bytes. The first byte in (DISP)
;         and the second byte in (DISP)+1.
;         (OUTPTR) <~ (OUTPTR) +1
;         (DISP) <~ (DISP)+2
; Reg affected: AF
; Call: CONVER CURSOR .

CHRWR:
        PUSH    HL
        PUSH    DE
        LD      HL, (OUTPTR)
        LD      (HL) ,A
        INC     HL
        LD      (OUTPTR) ,HL
        CP      9
        JR      Z,TABOUT
        CALL    CONVER
TAB_RET:
        CALL    CURSOR
        POP     DE
        POP     HL
        RET
;
;**************************************************************
;Function: Print out all the contents in input buffer
;          Check the Tv interface ,if TV interface
;          board exists then jump to TV interface
;          service routine.
;          There are four kinds of CRX as follows:
; Input: (OUTPTR) -- Point to the result address in input buffer.
; Output: (OUTPTR) <- INPBF
;         (DISP)   <- DISPBF
; Reg affected: AF .
; Call: CR0 PTEST PRINTT CLEAR CURSOR .

CR:
        LD      A,5
CR4:
        LD      (CRSET) ,A
        PUSH    HL
        LD      HL, (OUTPTR)
        LD      (HL) ,0DH
        CALL    CR0             ;Check Tv interface.
        CALL    PTEST           ;Check printer interface. ---- page 45 ----
        JR      Z,CR5
        LD      A, (CRSET)
        CP      46H
        JR      NZ,CR5
        LD      B,A
DELAY:  CALL    SCAN1
        DJNZ    DELAY
CR5:    CALL    PRINTT          ;Print message.
        POP     HL
        LD      A, (CRSET)
        CP      20H
        RET     Z
        CP      38H
        JP      Z,CLRBF
        CALL    CLEAR
        CALL    CURSOR
        RET
CR0:                            ;Routine for TV interface.
        LD      A, (TVSET)
        CP      0A5H
        JP      Z,TV
        RET
;
;**************************************************************
; Function: Same as CR but the display timing is about 1 sec,
; Input: (OUTPTR) -- Point to the result address in input buffer.
; Output: (OUTPTR) <- INPBF
;         (DISP)   <- DISPBF
; Reg affected: AF AF' BC' DE' HL' ,HL.
; Call: CR0 PTEST SCAN] PRINTT CLEAR CURSOR .

CR1:
        CALL    DECDSP
        LD      A,40H
        JR      CR4
;
;**************************************************************
; Function: Same as CR but CR2 do not call CLEAR and CURSOR.
; Input: (OUTPTR) -- Point to the result address in input buffer.
; Output: None
; Reg affected: AF
; Call: CR0 PTEST PRINTT .

CR2:
        LD      A,26H
        JR      CR4
;
;**************************************************************
; Function: Same as CR but CR3 call routine CLRBF insted of CLEAR .
; Input: (OUTPTR) -- Point to the result address in input buffer.
; Output: (OUTPTR) <~ (OUTPTR)+1
; (DISP) <~ (DISP)+2
; Reg affected: AF IX .
; Call: CR0 PTEST CLRBF .
;
CR3:
        LD A,39H
        JR CR4                                        ;---- page 46 ----     

;**************************************************************
; Routine for TAB key
;
TABOUT:
        LD      HL, (DISP)
        LD      DE,DISPBF+72
        AND     A
        SBC     HL,DE
        JR      NC, TAB_QQ
        LD      A,' '
        CALL    CONVER
        CALL    TAB_Q
        JR      NZ, TABOUT
        JR      TAB_RET
TAB_QQ: 
        LD      HL, (OUTPTR)
        DEC     HL 
        LD      (OUTPTR) ,HL
        JR      TAB_RET

TAB_Q:                           ;Check if cursor at TAB position.
                                ;Zero flag :Set if yes.

        LD      DE,DISPBF
        LD      HL, (DISP)
        AND     A
        SBC     HL,DE
        LD      A,L
TAB_Q_LP:
        RET     Z
        SUB     12
        JR      NC, TAB_Q_LP
        RET
;
;**************************************************************
; Function: Clear the display buffer and set the contents
;           of (DISP) and (OUTPTR) to the starting address
;           of display buffer and input buffer respectively.
; Input: None
; Output: (OUTPTR) <~ INPBF
;         (DISP) <~ DISPSF
;         Set all the contents of display buffer to be FF .
; Reg affected: None
; Call: CLRDSP

CLEAR:
        PUSH    HL
        LD      HL, INPBF
        LD      (OUTPTR) , HL
        LD      HL,DISPBF
        LD      (DISP) , HL
        POP     HL
        JP      CLRDSP
;
;**************************************************************
; Function: Convert ASC II codes to display patterns until
;           <CR> met.                                  ---- page 47 ----
;           Use HL as a pointer , convert the ASC II codes to
;           display patterns and stored them into display buffer.
; Input: HL -- Starting address of characters.
;        (OUTPTR) -- Point to the result address in input buffer.
;        (DISP) -- Point to the result address in display buffer.
; Output: HL <~ HL+?
;         (OUTPTR) <~ (OUTPTR)+?
;         (DISP) <- (DISP)+2*?
;         ? is the number of cheracters to be printed.
; Reg affected: AF HL .
; Call: CHRWR

MSG:
        LD      A, (HL)
        INC     HL
        CP      0DH
        RET     Z
        CALL    CHRWR 
        JR      MSG
;
;**************************************************************
; Function: Get a string of characters and end with <CR> .
; Input:
;       (OUTPTR) -- Point to the result address in input buffer.
;       (DISP) -- Point to the result address in display buffer.
; Output: (INPTR) <- (OUTPTR)
;         (OUTPTR) <- (OUTPTR)+?
;         (DISP) <- (DISP)+2*?
;         ? is the number of input characters. If the input
;         characters contains TAB code,then condition 2*? fails.
;         (COUNT) -- Number of characters including <CR> .
;         Zero flag -- Set if only <CR> is depressed.
; Reg affected: AF BC DE HL AF' BC' DE’ HL’ .
; Call: CHK40 CURSOR CR0 SACN CHRWR

READLN:

        LD      HL, (OUTPTR)
        LD      (INPTR) , HL    ; Set input pointer.
RDLOOP:
        CALL    CHK40   ;Adjust IX pointer.
        CALL    CURSOR
        LD      A, 50H
        LD      (CRSET),A
        CALL    CR0     ;Check TV interface.
        CALL    SCAN
        CP      11H
        JP      Z, ESCAPE       ;SOFTWARE ESCAPE (CONTROL Q).
        CP      0DH     ; CR
        JR      Z,RD_END
        CP      05FH    ; <--
        JR      Z,LEFT
        CP      5EH             ;UP arrow.
        JR      Z,RDLOOP
        CP      69H             ;DOWN arrow.
        JR      Z,RDLOOP
        LD      HL, (DISP)
        LD      DE, DISPBF+88   ;Check the numbers of character   ---- page 48 ----
                                ;in input buffer.
                                ;The numbers of input characters
                                ;is limited to 49.
        AND     A
        SBC     HL,DE
        JR      NC,RDLOOP
        CP      K_TAB           ;Check TAB key .
        JR      NZ ,NOTTAB
        LD      A,9            ;09 is the ASC II code for
                                ;TAB key .
NOTTAB:
        CALL    CHRWR
        JR      RDLOOP
RD_END:
        LD      HL, (OUTPTR)
        LD      (HL),A          ; Store 0DH .
        LD      DE, (INPTR)
        SBC     HL,DE           ; zero flag.
        INC     HL
        LD      A,L
        LD      (COUNT) ,A      ; Set /COUNT/
        RET
LEFT:                           ;Backspace key service routine.
        LD      HL, (INPTR)
        LD      DE, (OUTPTR)
        AND     A
        SBC     HL,DE
        JR      NC,RDLOOP       ; Ignore if exceeding LEFT end.
        EX      DE,HL
        DEC     HL              ;Decrease the pointer of
                                ;input buffer by one.
        LD      (OUTPTR) ,HL
        LD      A, (HL)
        CP      9
        JR      Z,B_TAB
        CALL    B_SP
        JR      RDLOOP
B_TAB:
        CALL    B_SP
        CALL    TAB_Q ;Check if cursor at TAB position.
        JR      Z,B_TAB1
        LD      HL, (DISP)
        DEC     HL
        LD      A, (HL)
        DEC     HL
        AND     (HL)
        INC     A
        JR      Z,B_TAB
B_TAB1:
        LD      HL, (OUTPTR)
B_TAB2:
        DEC     HL
        LD      A, (HL) 
        CP      ' '
        JP      NZ ,RDLOOP
        CALL    TAB_Q
        JP      Z,RDLOOP
        LD      A,' '           ;                 ---- page 49 ----
        CALL    CONVER
        JR      B_TAB2
B_SP:                           ;Clear the rightmost patterns
                                ;in display buffer.
        PUSH    HL
        CALL    DEC_SP
        DEC     HL
        DEC     HL
        DEC     HL
        LD      (DISP) , HL
        CALL    CURSOR
        POP     HL
        RET
;
;**************************************************************
; Function: Get cursor message.
; Input: (DISP) -- Point to the result address in display buffer.
; Output: The first byte of cursor in (DISP) and the second
;         byte of cusor in (DISP)+1
;        (DISP) <- (DISP) The content of (DISP) is unchanged.
; Reg affected: AF
; Call: CONVER

CURSOR:
        LD      A,05BH          ; PROMPT
CCURSOR:                        ; CALL HERE IF CHANGE PROMPT
        CALL    CONVER
        PUSH    HL
        LD      HL, (DISP)
        DEC     HL
        DEC     HL
        LD      (DISP) ,HL
        POP     HL
        RET
;
;**************************************************************
; Function: Convert binary data in HL to ASC II code and
; display patterns.
; Input: HL -~ Two bytes of hexadecimal values in HL.
; (OUTPTR) -~ Point to the result address in input buffer.
; (DISP) -- Point to the result address in display buffer.
; Output: Four ASC II code in (OUTPTR) ~ (OUTPTR)+3
; Eight bytes of display pattern in (DISP) ~ (DISP)+7
; (OUTPTR) <= (OUTPTR)+4
; (DISP) <- (DISP)+8
; Reg affected: AF
; Call: HEX2

HEXX:
        LD      A,H
        CALL    HEX2
        LD      A,L
        CALL    HEX2
        RET
;
;**************************************************************
; Function: Convert binary datas in HL to ASC II codes and
; display patterns.                                    ---- page 50 ----
; Call routint SPACE] to insert a space.
 ; Input: Same as HEXX
 ; Output: Five ASC II codes in (OUTPTR) - (OUTPTR)+4 .
; Ten bytes of display pattern in (DISP) ~ (DISP)+8 .
 ; (OUTPTR) <-— (OUTPTR)+5
 ; (DISP) <= (DISP)+19
 ; Reg affected: AF
 ; Call: HEXX SPACE] .

HEX4:
        CALL    HEXX
SPACE1: LD      A,' '
        JP      CHRWR
;
;**************************************************************
; Function: Convert binary data to ASC II code and
; display patterns.
; Input: A -- a byte in A register.
;             (OUTPTR) -- Point to the result address in input buffer.
;             (DISP) -- Point to the result address in display buffer.
; Output: The first ASC II code in (OUTPTR) and the second
;         ASC II code in (OUTPTR)+1 . Display patterns for
;         four bytes . The first byte in (DISP) and the
;         second byte in (DISP)+1 ,and so on.
;         (OUTPTR) <- (OUTPTR)+2
;         (DISP) <- (DISP)+4
; Reg affected: AF
; Call: HEX1

HEX2:
        PUSH    HL
        LD      HL, TEMP
        LD      (HL) ,A
        XOR     A
        RLD
        CALL    HEX1
        XOR     A
        RLD 
        CALL    HEX1
        POP     HL
        RET
;
;**************************************************************
; Function: Convert binary data to ASC II code and display
; pattern.
; Input: A -- LSB 4 bits contains the binary data.
;             (OUTPTR) -- Point to the result address in input buffer.
;             (DISP) -- Point to the result address in display buffer.
; Output: ASC II code in (OUTPTR).
;         Pattern for two bytes. The first byte in (DISP)
;         and the second byte in (DISP)+1 .
;         (OUTPTR) <- (OUTPTR)+1
;         (DISP) <- (DISP)+2
; Reg affected: AF
; Call: CHRWR

HEX1:
        ADD     A,'0'           ;                      ---- page 51 ----
        CP      '9'+1
        JR      C,HHH
        ADD     A,7
HHH:
        JP      CHRWR
;
;**************************************************************
; Function: Convert hexadecimal values in HL to corresponding
;           decimal format (in ASC II CODE format).
; Input: HL -- Hexadecimal values to be changed.
;        (OUTPTR) -- Point to the result address in input buffer.
;        (DISP) -- Point to the result address in display buffer.
; Output: (OUTPTR) <- (OUTPTR) +? :
;         (DISP) <- (DISP)+2*?
; Reg affected: AF BC DE HL IY .
; Call: CHRWR

DECIMAL:

        LD      IY,TENS         ; Table of ten's powers.
        LD      B,3             ; Output three digits.
        LD      C,0             ; Zero supress flag.
CLOOP:
        LD      E, (IY+0)
        INC     IY
        LD      D, (IY+0)
        INC     IY
        XOR     A
DECLOOP:
        SBC     HL,DE
        JR      C,ADDBACK
        INC     A
        JR      DECLOOP
ADDBACK:
        ADD     HL,DE
        CALL    SUPRESS
        DJNZ    CLOOP
        RET
SUPRESS:
        AND     A
        JR      Z,YES_0         ; If zero then ckeck zero
                                ;supress flag.
        LD      C,A             ; Else
        ADD     A,30H           ; Convert to ASC II code format
        JP      CHRWR           ; and output.
YES_0:
        LD      A,C
        AND     A
        JR      Z,BLANK0        ; Supress leading zero .
PRINT0:
        LD      A,C
        JP      CHRWR
BLANK0:
        LD      A,B             ; Still check for last digit,
        DEC     A
        JR      Z,PRINT0        ; If last digit then print ‘'g'
        LD      A,' '
        JP      CHRWR           ;                      ---- page 52 ----

;**************************************************************
; Function: Convert ASC II codes to corresponding hexadecimal
;           values until. met none hexadecimal digit.
;           The return value is stored in HL.
; Input: DE -- Point to the first location of ASC II code
;              to be changed.
; Output: HL -- Return values (hexadecimal digits).
;               (HEXFLAG) is set if there exists a digit within
;               (‘A'..'F') or the last none hexadecimal character
;               is 'H' .
; Reg affected: AF BC DE HL.
; Call: ONE

HEXBIN:

        XOR     A
        LD      (HEXFLA) ,A
        LD      B,A
        LD      H,A
        LD      L,A 
HBLOOP:
        LD      A, (DE)
        CALL    ONE
        JR      C,HQ
        ADD     HL, HL          ; (HL]=16* [HL]
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL, HL
        LD      C,A
        ADD     HL, BC
        INC     DE
        JR      HBLOOP
HQ:
        LD      A, (DE)
        CP      'H'
        RET     NZ
        INC     DE
        LD      (HEXFLA) ,A
        LD      A, (DE)
        RET


;
;**************************************************************
; Function: Convert a byte (ASC II code) in A register to
;           hexadecimal digit. :
; Input: A -- ASC II code.
; Output: A -- Hexadecimal values.
;         Carry flag = 1 If the data is not a hexadecimal digit.
;         (HEXFLAG) is not zero If the content of A within ‘A'
;         and 'F',
; Reg affected: AF
; Call: None

ONE:
        CP      'F'+1
        CCF                     ;                      ---- page 53 ----
        RET     C
        SUB     '0'
        RET     C
        CP      10
        CCF
        RET     NC
        SUB     7
        CP      18
        RET     C
        LD      (HEXFLA) ,A
        RET
;
;**************************************************************
; Function: Convert ASC II codes to corresponding decimal values
; in binary until met non decimal digits.
; Input: DE -- Point to the first ASC II code (Decimal format)
; to be changed.
; Output: HL ~- Return values (Decimal digits).
; Reg affected: AF BC DE HL.
; Call: None

DECBIN:

        LD      HL,0
        LD      A, (DE)
NDIGIT:
        SUB     '0'
        RET     C
        CP      10
        RET     NC
        ADD     HL,HL ; [HL]=19* {HL]
        LD      B,H
        LD      C,L
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,BC
        LD      B,0
        LD      C,A
        ADD     HL,BC
        INC     DE
        LD      A, (DE)
        JR      NDIGIT
;
;**************************************************************
; Function: Skip TABs and BLANKs.
; Input: HL -- Address to be check.
; Output: HL <- HL+? (? is the numbers of TAB and BLANK).
;                     and (HL) is not TAB or BLANK.
;         A <= (HL)
;         Carry flag = 0 If (HL) is between 'A' and 'Z'.
; Reg affected: AF HL .
; Call: None

SKIP:
        LD      A, (HL)
        CP      ' '
        JR      Z,SK1
        CP      9H              ; TAB                  ---- page 54 ----
        RET     NZ
SK1:
        INC     HL
        JR      SKIP
A_Z?:
; RETURN C-FLAG IF [A] IS NOT WITHIN {'A'..‘Z'}
        CP      'A'
        RET     C
        CP      'Z'+1
        CCF
        RET

;KEY CODE FOR DEPRESSED KEY
KEYTAB:
K0:     DEFB    31H             ;1
K1:     DEFB    41H             ;A
K2:     DEFB    20H             ; SPACE
k3:     DEFB    32H             ;2
k4:     DEFB    53H             ;S
K5:     DEFB    5FH             ;<--
K6:     DEFB    33H             ;3
K7:     DEFB    44H             ;D
K8:     DEFB    68H             ;-~>
K9:     DEFB    34H             ;4
KA:     DEFB    46H             ;F
KB:     DEFB    69H             ;DOWN ARROW
KC:     DEFB    35H             ;5
KD:     DEFB    47H             ;G
KE:     DEFB    5EH             ;UP ARROW
KF:     DEFB    36H             ;6
K10:    DEFB    48H             ;H
K11:    DEFB    0DH             ;CR
K12:    DEFB    37H             ;7
K13:    DEFB    4AH             ;J
K14:    DEFB    2FH             ;/
K15:    DEFB    38H             ;8
K16:    DEFB    4BH             ;K
K17:    DEFB    3CH             ;<
K18:    DEFB    39H             ;9
K19:    DEFB    4CH             ;L
KK3E:   DEFB    3EH             ;>
K1B:    DEFB    30H             ;8
KIC:    DEFB    3AH             ;2
KID:    DEFB    7BH             ;UNUSED
KIE:    DEFB    51H             ;Q
KIF:    DEFB    5AH             ;Z
KK12:   DEFB    2DH             ;-
K21:    DEFB    57H             ;W
K22:    DEFB    58H             ;X
KK13:   DEFB    3BH             ;;
K24:    DEFB    45H             ;E
K25:    DEFB    43H             ;C
KK19:   DEFB    40H             ;@
K27:    DEFB    52H             ;R
K28:    DEFB    56H             ;V
KK14:   DEFB    5BH             ;
K2A:    DEFB    54H             ;T
K2B:    DEFB    42H             ;B                     ---- page 55 ----
KK15:   DEFB    2BH             ;+
K2D:    DEFB    59H             ;Y
K2E:    DEFB    4EH             ;N
KK24:   DEFB    3DH             ;=
K30:    DEFB    55H             ;U
K31:    DEFB    4DH             ;M
K32:    DEFB    7BH             ;UNUSED
K33:    DEFB    49H             ;I
K34:    DEFB    2CH             ;,
K35:    DEFB    7BH             ;UNUSED
K36:    DEFB    4FH             ;O
K37:    DEFB    2EH             ;.
K38:    DEFB    7BH             ;UNUSED
K39:    DEFB    50H             ;P
K3A:    DEFB    3FH             ;?
K7B:    DEFB    7BH             ;UNUSED
RTABLE:
        DEFB    41H             ;A
        DEFB    46H             ;F
        DEFB    42H             ;B
        DEFB    43H             ;C
        DEFB    44H             ;D
        DEFB    45H             ;E
        DEFB    48H             ;H
        DEFB    4CH             ;L
        DEFB    60H             ;A'
        DEFB    61H             ;F'
        DEFB    62H             ;B'
        DEFB    63H             ;c'
        DEFB    64H             ;D'
        DEFB    65H             ;E'
        DEFB    66H             ;H'
        DEFB    67H             ;L'
        DEFB    49H             ;I
        DEFB    58H             ;X
        DEFB    49H             ;I
        DEFB    59H             ;Y
        DEFB    53H             ;S
        DEFB    50H             ;P
        DEFB    50H             ;P
        DEFB    43H             ;C
        DEFB    49H             ;I
        DEFB    46H             ;F
SEGTAB:
        DEFW    0FFFFH          ;SPACE
        DEFW    0F1FEH          ;!
        DEFW    0F7DFH          ;"
        DEFW    0FC31H          ;#
        DEFW    0FC12H          ;$
        DEFW    0C31BH          ;%
        DEFW    0E724H          ;&
        DEFW    0FBFFH          ;!
        DEFW    0EBFFH          ;(
        DEFW    0D7FFH          ;)
        DEFW    0C03FH          ;*
        DEFW    0FC3FH          ;+
        DEFW    0DFFFH          ;,
        DEFW    0FF3FH          ;-                     ---- page 56 ----
        DEFW    0BFFFH          ;.
        DEFW    0DBFFH          ;/
        DEFW    0DBC0H          ;0
        DEFW    0FCFFH          ;1
        DEFW    0FF24H          ;2
        DEFW    0FF30H          ;3 
        DEFW    0FF19H          ;4
        DEFW    0F772H          ;5
        DEFW    0FF02H          ;6
        DEFW    0FFF8H          ;7
        DEFW    0FF00H          ;8
        DEFW    0FF10H          ;9
        DEFW    0EF7FH           ;:
        DEFW    0DFBFH          ;;
        DEFW    0DBF7H          ;<
        DEFW    0FF37H          ;=
        DEFW    0E7F7H          ;>
        DEFW    0FD7CH          ;?
        DEFW    0FDA0H          ;@
        DEFW    0FF08H          ;A
        DEFW    0FC70H          ;B
        DEFW    0FFC6H          ;C
        DEFW    0FCF0H          ;D
        DEFW    0FF06H          ;E
        DEFW    0FF0EH          ;F
        DEFW    0FF42H          ;G
        DEFW    0FF09H          ;H
        DEFW    0FCF6H          ;I
        DEFW    0FFE1H          ;J
        DEFW    0EB8FH          ;K
        DEFW    0FFC7H          ;L
        DEFW    0F3C9H          ;M
        DEFW    0E7C9H          ;N
        DEFW    0FFC0H          ;O
        DEFW    0FF0CH          ;P
        DEFW    0EFC0H          ;Q
        DEFW    0EF0CH          ;R
        DEFW    0FF12H          ;s
        DEFW    0FCFEH          ;T
        DEFW    0FFC1H          ;U
        DEFW    0DBCFH          ;V
        DEFW    0CFC9H          ;W
        DEFW    0C3FFH          ;X
        DEFW    0F1FFH          ;Y
        DEFW    0DBF6H          ;Z
        DEFW    0CFFFH          ;^
        DEFW    0E7FFH          ;/
        DEFW    0FFF0H          ;]
        DEFW    0CDFFH          ;^
        DEFW    0EB7FH          ;<
        DEFW    0BF08H          ;A'
        DEFW    0BF0EH          ;F'
        DEFW    0BC70H          ;B'
        DEFW    0BFC6H          ;C'
        DEFW    0BCF0H          ;D'
        DEFW    0BF06H          ;E'
        DEFW    0BF09H          ;H'
        DEFW    0BFC7H          ;L'                    ---- page 57 ----
SHIFTT:        
        DEFB    3CH             ;<
        DEFB    0FFH
        DEFB    3EH             ;>
        DEFB    0FFH
        DEFB    2AH             ;*
        DEFB    21H             ;!
        DEFB    22H             ;"
        DEFB    23H             ;#
        DEFB    24H             ;$
        DEFB    25H             ;%
        DEFB    26H             ;&
        DEFB    27H             ;'
        DEFB    28H             ;(
        DEFB    29H             ;)
        DEFB    3BH             ;;
        DEFB    0FFH
        DEFB    0FFH
        DEFB    0FFH
        DEFB    0FFH
        DEFB    2FH             ;/
        DEFB    0FFH
        DEFB    0FFH
        DEFB    0FFH
        DEFB    0FFH
        DEFB    0FFH
        DEFB    0FFH
        DEFB    0FFH
        DEFB    0FFH
        DEFB    0FFH
        DEFB    2DH             ;-
        DEFB    0FFH
        DEFB    5BH             ;^
        DEFB    40H             ;@
        DEFB    0FFH
        DEFB    0FFH
        DEFB    3DH             ;=
        DEFB    2BH             ;+
MPFII:
        DEFM    '*****MPF'
        DEFB    2DH             ;-
        DEFM    'I'
        DEFB    2DH             ;-
        DEFM    'PLUS*****'
        DEFB    0DH
ERR_SP:
        DEFM    'ERROR'
        DEFB    2DH             ;_
        DEFM    'sp'
        DEFB    0DH
SYS_SP:
        DEFM    'SYS'
        DEFB    2DH             ;_
        DEFM    'SP'
        DEFB    0DH
PRTON:
        DEFM    'PRT ON'
        DEFB    0H              ;                      ---- page 58 ----
PRTOFF:
        DEFM    'PRT OFF'
        DEFB    0DH
RAM2K_VALUE_SET:
        DEFW    0F800H          ;SET EDITOR LIMITS.
        DEFW    0FCFFH
        DEFW    0FE00H          ;SET SYMBOL LIMITS.
        DEFW    0FEA0H
        DEFW    0FD00H          ;SET OBJECT LIMITS.
        DEFW    0FDFFH
RAM4K_VALUE_SET:
        DEFW    0FA00H          ;SET EDITOR LIMITS.
        DEFW    0FAFFH
        DEFW    0FD00H          ;SET SYMBOL LIMITS.
        DEFW    0FEA0H
        DEFW    0FB00H          ;SET OBJECT LIMITS.
        DEFW    0FCFFH
TENS:                           ; TABLE USED BY 'TOASCII' TO CONVERT
                                ; BINARY TO DECIMAL DIGITS
        DEFW    100
        DEFW    10
        DEFW    1
        DEFM    ' ERRORS'
        DEFB    0DH             ;                      ---- page 59 ----
;*HEADING RAM STORAGE .
;*******************************;
;*                             *;
;*        STORAGE              *;
;*      FOR MONITOR            *;
;*                             *;
;*******************************;
;
; 
        ORG     0FEA0H
USERSTK:
        DEFS    30H
        ORG     0FED0H
SYSSTK:
STEPBF: DEFS    9
TEXT_F:                          ;ASSEMBLER SOURCE FROM.
EDIT_START_ADDR:DEFS    2       ;EDITOR BOTTOM.
TEXT_T:                          ;ASSEMBLER SOURCE TO.
END_DATA_ADDR:  DEFS    2       ;EDITOR TOP.
END_LN_NO:      DEFS    2       ;EDITOR LAST LINE NUMBER.
RAM_START_ADDR: DEFS    2       ;EDITOR LOW LIMIT.
EDIT_END_ADDR:  DEFS    2       ;EDITOR HIGH LIMIT.
ST_F:           DEFS    2       ;ASSEMBLER SYMBOL TABLE FROM.
ST_T:           DEFS    2       ;ASSEMBLER SYMBOL TABLE TO.
OBJ_F:          DEFS    2       ;ASSEMBLER OBJECT CODE FROM.
OBJ_T:          DEFS    2       ;ASSEMBLER OBJECT CODE TO.
END_ADDR:       DEFS    2       ;Contains the limit address
                                ;of ccommand INSERT or DELETE .
BRAD:   DEFS    2               ;Breakpoint address .
BRDA:   DEFS    1               ;Data of breakpoint address .
POWERUP:DEFS    1               ;Power_up initialization .
TEST:   DEFS    1               ;Bit 7 -- set when illegal key
                                ;         is entered.
STEPFG: DEFS    1               ;STEP mode test flag .
PRTFLG: DEFS    1               ;Printer toggle switch .
BEEPSET:DEFS    1               ;Beep sound toggle switch.
FBEEP:  DEFS    1               ;Freqency of BEEP .
TBEEP:  DEFS    2               ;Time duration of BEEP .
MADDR:  DEFS    2               ;Temporary storage .
TEMP1:  DEFS    4               ;See comments on command STEP .
ATEMP:  DEFS    1               ;Temporary storage .
HLTEMP: DEFS    2               ;Temporary storage .
IM1AD:  DEFS    2               ;Contains the address of Opcode ‘FF’
                                ;service routine.( RST 38H, mode
                                ;1 interrupt, etc).
RCOUNT: DEFS    1               ;Register counts in register table.
INPBF:  DEFS    40              ;Input buffer .
DISPBF: DEFS    82              ;Display buffer .
GETPT:  DEFS    2               ;Temporary storage for GETHL .
TYPEFG: DEFS    1               ;Type test flag.
CRSET:  DEFS    1               ;Display delay time .
OUTPTR: DEFS    2               ;Input buffer pointer.
DISP:   DEFS    2               ;Display buffer pointer .
INPTR:  DEFS    2               ;Limit of input buffer pointer .
REGBF:
USERAF: DEFS    2
USERBC: DEFS    2
USERDE: DEFS    2               ;                      ---- page 60 ----
USERHL: DEFS    2
UAFP:   DEFS    2               ;AF'
UBCP:   DEFS    2               ;BC'
UDEP:   DEFS    2               ;DE'
UHLP:   DEFS    2               ;HL'
USERIX: DEFS    2
USERIY: DEFS    2
USERSP: DEFS    2
USERPC: DEFS    2
USERIF: DEFS    2
BLANK:  EQU     6FD0H
K_TAB:  EQU     68H             ;TAB CODE
TVSET:  EQU     0A000H          ;The first memory location
                                ;of TV interface board .
TV:     EQU     0A001H          ;The starting address of monitor
                                ;program on TV interface board .
BASICC: EQU     2020H           ;The starting address of
                                ;reenter BASIC .       ---- page 61 ----

; Assembly additions for z80asm, values retrieved from ROM disassembly:
TEMP:   EQU     0ffach; 0000H
HEXFLA: EQU     0fffbh; 0000H
COUNT:  EQU     0ffabh; 0000H
CHK40:  EQU     00912H
MTPPRT: EQU     06a40h; 0000H
ERRSMSG:EQU     00CAFH
MDUMP:  EQU     06500h; 0000H
TEST5:  EQU     0ffcch; 0000H
PRT_MPF:EQU     06990h; 0000H
BASICZ: EQU     02000h; 0000H
REEDIT: EQU     00cf2h; 0000H
EDIT:   EQU     00ce6h; 0000H
LASM:   EQU     01052h; 0000H
ASM:    EQU     010cch; 0000H

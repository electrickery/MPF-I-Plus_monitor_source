;*********************************************************
;*                                                       *
;*     COPYRIGHT, MULTITECH INDUSTRIAL CORP. 1983        *
;*     ALL right reserved.                               *
;*     No part of this software maybe copied without     *
;*    the express written consent of MULTITECH           *
;*    INDUSTRIAL CORP.                                   *
;*                                                       *
;*********************************************************
;
;
;
;
;
P82551	EQU	83H		;8255 I control port
DIG1	EQU	80H		;8255 I port A
DIG2	EQU	81H		;8255 I port B
DIG3	EQU	82H		;8255 I port C
P82552	EQU	93H		;8255 II Control port
SEG1	EQU	90H		;8255 II port A
SEG2	EQU	91H		;8255 II port B
KIN		EQU	92H		;8255 II port C
PWCODE	EQU	0A5H	;Power up code
ZSUM	EQU	0E0H	;This will make the sum of all
					;monitor codes to be zero.

; The following EQUATES are used for timing. Their values
; depend on the CPU clock frequency. (In this version, the
; crystal freqency is 1.79MHz.)

COLDEL	EQU	80		; Column delay for routine
					; SCAN and SCAN1.
F1kHZ	EQU	65		; Delay count for 1K Hz square wave,
					; used by routine TONE1K.
F2KHZ	EQU	31		; Delay count for 2K Hz square wave,
					; used by routine TONE2K.
MPERIOD	EQU	32		; 1KHz and 2KHz threshold used by
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

ONE_1K	EQU	4
ONE_2K	EQU	4
ZERO_1K	EQU	2
ZERO_2K	EQU	8

;**************************************************************
;I/0 port assignment: (8255 I)
;                                                  ----P2----
; port A (address 80H): The first eight digits of display
; bit0--digit 1
; bit1--digit 2
; bit2--digit 3
; bit3--digit 4
; bit4--digit 5
; bit5--digit 6
; bit6--digit 7
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

;Thefollowingbytemakesthesumofthe monitor
;code in ROM zero.

        DEFB    ZSUM
                            ;                             ---page 4 ----
;**********************************************************

        ORG     38H

; EntrypointofRST38H(opcodFF)ormode1interrupt.
; FetchtheaddressstoredinlocationFF00andFF01,
; thenjumptothisaddress.Initially,FF00andFF01
; aresetto0066.SoRST 38 willhavethesameeffect
; as softwarebreak.BychangingthecontentofFF00
; and FF01, the user candefinehisorher own service
; routine.
; The nextthreeinstructions push the contentsofFF00
; andFF01tostackwithoutchanginganyregisters.

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
; Thisisapartofbreakserviceroutine.Itcontinues
; theprogramatRST28.

        LD      (ATEMP),A

; Themonitorhaschangedthecontentofuser's
; program atbreakaddress.Thenext3instructions
; restoredthedestroyedcontent.BRADcontainsthe
; breakaddress,BRDAcontainstheoriginaldataat
; breakaddress.

        LD      HL,(BRAD)
        LD      A,(BRDA)
        LD      (HL),A
; Sendbreakenablesignaltohardwr&counte1.
; AnonmaskableinterruptwillbeissuedatthethMi's.

        LD      A,11101111B
        OUT     (KIN),A
        LD      A,(ATEMP)   ; 1st M1
        LD      HL,(HLTEMP) ; 2nd M1
        NOP                 ; 3rd M1
        RET                 ; 4th M1

; Return to user's program.Executethe1nstruct100
; atbreakaddress,Afterfinishingoneinstruction,
; anonmaskableinterrupthappensndthenreturns
; tothemonitorprogramagain.
;
RESET2:
        LD      (USERIF),HL ; Setuser'sIregisterand
                            ; interuptflipfloptoH       ---- page 5 ----
        LD      (TEST),HL   ; SetthecontentsofTESTand
                            ; STEPBFtobezero.

; TEST isaflagfortheuseofmonitoritself.Illegalkey-in
; blanking(bit7ofTEST)andautomaticleadingzero
; (bit0)usethisflag.Clearithere.

        LD      HL,USERSTK
        LD      (USERSP),HL
        CALL    INI7
        SCF

; Address66Histhe addressfornonmaskableinterrupt.
; Skipthisarea,monitorresumesat SETST0

        JR      SETST0
;
;**************************************************************
NMI:    ORG     66H

; Entrypoint ofnonmaskableinterrupt.NMIwilloccur
; whenuser'sprogramisbreaked.
; The serviceroutinewhichstartsheresavesall
; user'sregistersandstatus.Italso checkthevalidity
; ofuser'sSP.

        LD      (ATEMP),A       ;SaveAregister
        LD      A,0FFH  ;DisableBREAKsignaland all digits.
        LD      (DIG1),A
        LD      (DIG2),A
        LD      (DIG3),A
        OUT     (KIN),A
        LD      A,ATEMP         ;RestoreAregister
RGSAVE: LD      (HLTEMP),HL     ;SaveregisterHL
        LD      POP HL          ;Getreturnaddressfromstack
        LD      (USERPC),HL     ;Setuser'sPCtoreturn
                                ;address
        LD      HL,(HLTEMP)     ;RestoreHLregister
        LD      (USERSP),SP     ;etuser•SPtocurrentSP
        LD      SP,USERIY+2     ;Saveotherregistersby
        PUSH    IY              ;continuouslypushingthem
        PUSH    IX              ;ontostack
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
        
; ThenexttwoinstructionssaveIregister.
; Theinterruptflip-flop(IFF2)iscopiedinto
; parityflag{P/V)byinstructionLDA,I.                    ---- page 6 ----
; Theiriterruptstatus(enabledordisabled)
; canbedeterminedbytestingparityflag.

        LD      A,I
        LD      (USERIF+1),A

; ThenextfourinstructionssaveIFF2into
; user's IFF.
        LD      A,0
        JP      PO,SETIF        ;PO--P/V=0
        LD      A,1
SETIF:  LD      (USERIF),A
;

        LD      SP,SYSSTK       ;Set SPtosystem stack.
        
; Thenext7instructionscheckuser'sSP.
; Iftheuser'sSPpointstoalocationnot
; inRAM,displayERR-SP.

        LD      HL,(USERSP)
        DEC     HL
        CALL    RAMCHK
        JR      NZ,SETST2
        DEC     HL
        CALL    RAMCHK
        JR      NZ,SETST2

; Iftheuser'sstackandsystemstackare
; overlayed,thendisplaySYS-SP.Thischecking
; isdonebythefollowinginstructions.

        LD      DE,-USERSTK+1
        ADD     HL,DE
        JR      C,SETST3
SETST0:
        LD      A,(BRDA)
        LD      HL,(BRAD)	;Restorethedata atbreakpoint
                            ;address.
        LD      (HL),A

; Inexecutionof STEPor GO command,ifthe
; user'sSPislegal(carryflagiszero)then
; displayuser'sPCandthe firstfour
; registercontents.
; UsercanusetheUPorDOWNkeystocheck
; theregistercontents.
; Otherwise,displayfixedmessage(ERR-SP
; orSYS-SP)

        CALL    NC,MEMDP2
;
;
;**************************************************************
; Scanthe•display andkeyboard. When a keyis
; detected,takeproperactionaccordingtothe
; keypressed.
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
        JR      Z,FOR       ;DOWN ARROW.
        CP      5EH
        JR      Z,BACK      ;UP ARROW.
        CP      'M'         
        JR      Z,MEMEXEC   ;MEMORY DISPLAY AND MODIFY.
        CP      'R'
        JR      Z,REGEXC    ;REGISTER DISPLAY AND MODIFY.
        CP      'L'
        JR      Z,LOAD      ;TAPE READ.
        CP      'W'
        JR      Z,DUMP      ;TAPE WRITE
        CP      'G'
        JR      Z,GOEXEC    ;EXECUTION
        CP      'S'
        JR      Z,STEP      ;SINGLE STEP.
        CP      'B'
        JR      Z,BREAK     ;BREAK AT A SPECIFIED ADDRESS.
        CP      'F'
        JR      Z,FILLDA    ;FILL DATA.
        CP      'I'
        JR      Z,INSET     ;INSERT A BLOCK OF DATA.
        CP      'D'
        JR      Z,DELETE    ;DELETE ONE BYTE OF DATA.
        CP      'J'
        JR      Z,JUMP      ;JUMP RELATIVE.
        CP      1
        JR      Z,ASM       ;ASSEMBLER (CONTROL A).
        CP      0CH
        JR      Z,LASM      ;LINE ASSEMBLER (CONTROL L).
        CP      2
        JR      Z,BASIC3    ;ENTER BASIC (CONTROL B).
        CP      3
        JR      Z,BASIC3    ;REENTER BASIC (CONTROL C).
        CP      4
        JR      Z,DEASM3    ;DISASSEMBLER (CONTROL D).
        CP      5
        JR      Z,EDIT      ;EDITOR (CONTROL E).
        CP      7
        JR      Z,BEEP      ;BEEP SOUND CONTROL (CONTROL G).
        CP      12H
        JR      Z,REEDIT    ;REEDIT (CONTROL R).
        CP      10H
        JR      Z,PRT_CONTROL;PRINTER CONTROL (CONTROL P).  ---- page 9 ----
        CP      0DH
        JP      Z,CR3       ;LINEFEED.
        
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
RAMT1   LD      BC,8000H
RAMT2   CALL    RAMCHK
        JR      Z,TNEXT
        JR      INI8
TNEXT   CPD
        JP      PE,RAMT2

;Thenext.four instructionssetthedefaultvaluesaccording
;toEDITORand ASSEMBLERrespectively.

        LD      HL,RAM4K_VALUE_SET
INI6:
        LD      DE,RAMSTARTADDR
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
        LD      HL,PBEEP
        LD      (HL),44H    ;Frequency of BEEP.
        INC     HL
        LD      (HL),2FH    ;Time duration of BEEP.
        INC     HL
        LD      (HL),0
INI4:   LD      NMI
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
        LD      (OUTPRT),HL
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
        CALL    SCAN1           ;Get position code.
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

KSHIFT;
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
        EX      AF.AF'
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
KCOL:   LD      A,(IX)
        OUT     (SEG1),A        ;Firstbytepattern.
        INC     IX
        LD      A,(IX)
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
        JP      Z,MMODIFY
        CP      2EH             ;.
        JP      Z,MDUMP1
        CP      2FH             ;/
        JR      Z,MMOVE
        CALL    MEMEX3          ;Displayspecifiedmemory
                                ;contents.
        CALL    HEXX
P102    CALL    MEM3
        CALL    CR2
        RET

;**************************************************************

;Theinputdataustbehexadecimalvalues.MPF_iPwill
;ignorethiscommandifthereexistsatleastonedigit
;whichisnotahexadecimalvalue.Theusercanusethe
;BACKSPACEkey tocorrectthedata.

MEMEX2:
        CALL    ECHOCH          ;Echothe inputcharacterand
                                ;prompt.
MEMEX1  CALL    GET             ;Getastringofcharacters
                                ;andendtheinputwith <CR>.
        CALL    CHKINP          ;Checkhexadecimalvalues.
        JR      C,MEMEXl        ;JumptoMEMEXliftheinput
                                ;                      ---- page 17 ----

986


;data isillegal.

0321

0324
0327
CDDF08

22F8FEC9
987
988
989
990
991
992
993
CALL

LDRET
CHKHEX(MADDR),HL
Get the hexadecimal addressof the first of four memorylocationstobedisplayed,

1045
1046
1047
;Thefinalcommandlooklikethis:
;<M>=<start>/<end><destination><CR>















1069	;ending-address<startingaddress.)


.










0382
0384
0385


EDB0
lBC9

1086
1087
1088
1089
1090


LDIR
DEC	DERET

;SBCHL,DE.RestoreHL.
;Blocktranster
;DE=lastaddressmoved.
1091
1092
1093
1094
·,**************************************************************
;Todisplayfourconsecutivememorycontents.

1095
MEM3:

0386
0389
2AF8FE
0604
1096
1097
1098

MEMS:
LO
LD
HL,(MADDR)
B,4
038B
CD950A
1099
CALL
SPACEl
;Insertaspace.
038E
7E
1100
LD
A,(HL)

038F
CD9A0A
1101
CALL
HEX2

0392
23
1102
INC
HL





0393	10F6	1103

DJNZ
MEMS

1104
DECDSP:


Cleartherightmostdisplay
1105



pattern.
1106



inDISPLAYBUFFER.Thedisplay
1107
0395	DD212CFF	1108
0399	3EFF	1109

DECSP
LDLD
IX,DISPBFA,0FFH
patternisusuallya cursor.
0398
2A84FF
1110
LD
HL,(DISP)
039E
77
1111
LD
(HL),A
039F
23
1112
INC
HL
03A0
77
1113
LD
(HL),A
03Al
C9
1114
RET



1115
1116
·,**************************************************************


1117



1118
;ExecutedwhenUPordownarrowkeyispressed.


1119
;E·xecutedinmemorymodeonly.


1120



1121
MFOR:	;Displaynextfourmemory


1122
;contents.
03A2
2AF8FE
1123
LD	HL,(MADOR)
03A5
23
1124
INC	HL
03A6
23
1125
INC	HL
03A7
23
1126
INC	HL
03A8
23
1127
INC	HL
03A9
22F8FE
1128
Pl01	LD	(MADDR),HL
03AC
3E4D
1129
LD	A,'M'
03AE
CD6008
1130
CALL	ECHOCH	;Getpattern'<M>='
03B1
CD890A
1131
CALL	HEXX-
0384
C30F03
1132
JP	Pl02


1133
MBACK:	;Displaylastfourmemory


1134
;contents.
03B7
2AF8FE
1135
LD	HL,(MADDR)
03BA
2B
1136
DEC	HL
038B
2B
1137
DEC	HL
038C
2B
1138
DEC	HL
03BD
2B
1139
DEC	HL
03BE
18E9
1140
JR	Pl01


1141
1142
·,**************************************************************


1143



1144
;Executedwhen'F'keyispressed.


1145
;Storethe databyteintoall memorylocationsfrom


1146
;addltoaddr2.


1147
;The finalcommandlooklike this:


1148
;F>=<addrl><addr2><data><CR>


1149



1150
FILLDA:
03C0
CD1603
1151
CALL	MEMEX2	;Getstartingaddress.
03C3
CD1908
1152
CALL	RAMCHK
03C6
C2C406
1153
JP	NZ,ERROR	;JumptoERRORifthe


1154
;memorylocationof the


1155
;startingaddressisnotRAM.
03C9
ES
1156
PUSH	HL
03CA
CDE508
1157
CALL	GETHL	;Get endingaddress.
03CD
ES
1158
PUSH	HL
03CE
CDE508
1159
CALL	GETHL	;Getdata.
03D1
7D
1160
LD	A,L




03D2
A7
1161
AND
A

03D3
El
1162
POP
HL

03D4
Dl
1163
POP
DE

03D5
12
1164
LO
(DE),A

03D6
ED52
1165
SBC
HL,DE

03D8
C8
1166
RET
z

03D9
DAC406
1167
JP
C,ERROR
;JumptoERRORifstarting


li68


;address>enoingaddress.
03DC
44
1169
LD
B,H

03DD
4D
1170
LO
C,L

03DE
62
1171
LO
.H,D

03DF
6B
1172
LO
L,E

03E0
13
1173
INC
DE

03El
EDB0
1174
LDIR


03E3
C38509
1175
JP
CR3



1176





1177
·,**************************************************************


1178



1179
;Exeutedwhen'I'keyispressed.


1180
;MPFIPwilldisplaythecurrentlimitaddress.


1181
;SYSTEMRAMdatathatofcoursecannotbeshifted


1182
;sowemustsetthelimitaddressofINSERTcommand.


1183
;Whenonebyteisinsertedatsomeaddress,all


1184
;databelowthisaddresswillbeshifteddownone


1185
;position.


1186
;Thelastlocationwillbeshiftedoutandthereforelost.


1187



1188
(1)Type<CR>	To seethecurrentlimitaddress


1189
ofINSERTcommand.


1190
(2)TypeC	Toclearlimitaddress(i.e.,set


1191
limitaddresstobe0FE00H).


1192
(3)Enterthehexadecimaladdress--Toset newhigh


1193
limitaddressofINSERTcommand.


1194



1195
;WhenMPFIPdisplay<I>=


1196
;Youcan-enterthehexadecimaladdressandvaluesseparated


1197
;	by spaces--Toinsertablockofdata.


1198
;Thefinalcommandlooklikethis:


1199
;<!>=<address><datal><data2><data3><data4><CR>


1200



1201



1202
INSET:
03E6
CD1504
1203
CALL	INSET4


1204
INSET3:
03E9
22D0FE
1205
LO	(STEPBF),HL
03EC
23
1206
INC	HL
03ED
22D4FE
1207
LD	(STEPBF+4),HL
03F0
ED5BEBFE
1208
LO	DE,(ENDADDR)
03F4
lB
1209
DEC	DE	-
03F5
ED53D2FE
1210
LO	(STEPBF+2),DE
03F9
13
1211
INC	DE
03FA
A7
1212
AND	A
03FB
ED52
1213
SBC	HL,DE
03FD
D2C406
1214
JP	NC,ERROR
0400
CDE508
1215
CALL	GETHL
0403
F5
1216
PUSH	AF


1217



1218
;RoutineGMVneeds3parameterswhicharestoredin




1219
;step-buffer(STEPBF):
1220
;STEPBF:startingaddress(2bytes).
1221
;STEPBF+2:	endingaddress(2bytes).
1222
;STEPBF+4:destinationaddress(2bytes).
1223








;Echotheinputcharacterwith
;<?>=
;?isIorDaccordingtoINSERT.
;commandorDELETEcommand
;respectively.
;Getthecurrentlimitaddress.
;/
0421
CD2409
1242

CALL
CHRWR
'<I>=<
currentlimitaddress>/
0424
37
1243

SCF



0425
3F
1244

CCF





1245
INSETl:




0426
CD6F06
1246

CALL
GET

Getastringofcharacters


1247




endtheinputwith<CR>.
0429
3A0DFF
1248
LD
A,(INPBF+9)
042C
2812
.1249

JR
Z,INSET2
;For<CR>condition•
042E
FE43
1250

CP
'C'

0430
CC1702
1251

CALL
Z,CLRI
;ForC condition.
0433
280B
1252

JR
Z,INSET2

0435
2108FF
1253

LD
HL,INPBF+4

0438
CDE208
1254

CALL
CHKHE2
;Getnewlimitaddress.
043B
38E9
1255

JR
C,INSETl
;JumptoINSET!iftheinput


1256



;datasisillegal.
043D
22EBFE
1257

LD
(END_ADDR),HL



1258
INSET2:



0440
CD8509
1259

CALL
CR3
;Printmessage.
04.43
Fl
1260

POP
AF

0444
CD1603
1261

CALL
MEMEX2

0447
C9
1262

RET


1263
1264
·,***************-*********************************************
1265












..









LOC

MPF IPOBJCODEMSTMTSOURCESTATEMENT

1983.1.1	PAGE23
ASM5.9

1277
1278
1279
1280
1281
1282
1283
limitaddress.

When MPF IP display <D>= you can enter the hexadecimaladdresswhichcontentyouwanttobedeleted.
Youcandeleteonebytefrommemoryatatime.
DELETE:







1294
1295
1296
1297
1298
1299
1300
1301
1302

·,**************************************************************
Executedwhen'J'keyispressed.
Instruction JR and DJNZ requires relative addresses.MPF IP supports the calculation of relative addressesthroughtthe 'J'command.
JUMP:







LOC

MPF IPOBJCODEMSTMTSOURCESTATEMENT

1983.1.1	PAGE24
ASM5.9

0479	12
1335
1336
1337
LD	(DE);A
Savetheoffsetinto
the next byte of opcode.(JRorDJNZ)
047A
C38509
1338
1339
1340
JP	CR3
;**************************************************************
..









LOC
MPFIPOBJCODEMSTMTSOURCESTATEMENT
1983.1.1	PAGE25
ASM5.9

04Cl
181C
1393
1394
1395i
JR	PREOUT
1396
1397
·I**************************************************************
1398
;Executedwhen'G' keyispressed.
1399
;Executionatspecifiedaddressorcurrentaddress.
1400
;Thefollowingroutineis theserviceroutinefor
1401
;'GO'key.
1402

th<G>=


1406


;endtheinputwith<CR>•
04C9
3A08FF
1407
LD
A,(INPBF+4)

04CC
FE0D
1408
CP
0DH
04CE
2808
1409

JR
Z,EXEC2
;Ifzero,thenexecuteat


1410



;thecurrentaddress.
0400
CDDF08
1411

CALL
CHKHEX
;Get specifiedaddress.
0403
38Fl
1412

JR
C,GOEXEl
;JumptoGOEXElifoneof


1413



;theinputdatumisillegal.
0405
229EFF
1414

LD
(USERPC),HL



1415
EXEC2:



0408
2AEDFE
1416

LD
HL,(BRAD)
;Gettheaddress ofbreakpoint.
040B
36EF
1417

LD
(HL),0EFH
;InstructionRST28H.


1418



;Thecontentofbreakaddress


1419



;ischangedtoRST28Hbefore


1420



;thecontrolistransferedto


1421



;user'sprogram.This


1422



;willcauseatrapwhenuser's


1423



;PCpassesthispoint.
040D
3EFF
1424

LD
A,0FFH
;SaveFFintoTEMPl. Thisdata


1425



;willbeoutputto portClater.


1426



;FFisusedtodisablebreakpoint.
04DF
32FAFE
1427
PREOUT:
LD
(TEMPl),A

04E2
3EA5
1428

LD
A,0A5H

04E4
32F2FE
1429

LO
(STEPFG),A

04E7
3AA0FF
1430

LD
A,(USERIF)
;Savetwoinstructionsinto


1431



;TEMPandTEMP+l.Thesetwo


1432



;instructionswillbeexecuted


1433



;later.Iftheuser'sIFF


1434



;(interruptflip-flop)is1,


1435



;theinstructionsare'EIRET'.


1436



;Otherwise,theyare'DIRET'.
04EA
CB47
1437

BIT
0,A

04EC
21FBC9
1438

LD
HL,0C9FBH
;'EI','RET'
04EF
2002
1439

JR
NZ,EIDI

04Fl
2EF3
1440

LD
L,0F3H
;'DI'


1441
EIDI:



04F3
22FBFE
1442

LD
(TEMPl+l),HL

04F6
3188FF
1443

LD
SP,REGBF
;Restoreuser'sregistersby


1444



;settingSPtoREGBF(register


1445



;buffer)andcontinuously


1446



;poppingthestack.
04F9
Fl
1447

POP
AF

04FA
Cl
1448

POP
BC

04FB
Dl
1449

POP
DE

04FC
El
1450

POP
HL








LOC

MPF IPOBJCODE MSTMTSOURCESTATEMENT

1983.1.1	PAGE26
ASM5.9

04FD
08
1451
EX
AF,AF'
04FE
Fl
1452
POP
AF
04FF
0500
0501
08
D9
Cl
1453
1454
1455
EXEXXPOP
AF,AF'
BC
0502
Dl
1456
POP
DE
0503
El
1457
POP
HL
0504
0505
D9DDEl
1458
1459
EXXPOP

IX
0507
FDEl
1460
POP
IY
0509
ED7B9CFF
1461
LD
SP,(USERSP)	;Restoreuser'sSP.
050D
3289FF
1462
LD
(USERAF+l),A
0510
3AA1FF
1463
LD
A,(USERIF+l)	;Restoreuser'sI•
0513
ED47
1464
LD
I,A
0515
ES
1465
PUSH
HL	;Thenext 3instructions


1466

;pushtheaddress being
1467
;displayednow(inUSERPC)
1468
;ontostackwithoutchanging
1469
;HLregister.Thisaddresswillbe


1470

;treatedasuser'snewPC.
0516
2A9EFF
1471
LD
HL,(USERPC)
0519
E3
1472
EX
(SP),HL
051A
3AFAFE
1473
LD
A,(TEMPl);Outputthedatastoredin


1474

;TEMPltoportCof8255II


1475

;Thisdataispreparedby


1476

;commandSTEPorGO.


1477

,Infirstcase,itis


1478

;11101111andwillenable


1479

;breakpoint.Inother


1480

;cases,itisFFandwill


1481

;disablebreakpoint.


1482

;Ifbreakisenabled,non­


1483

;maskableinterruptwilloccur


1484

;5Ml'saftertheOUTinstruction.
0510
D392
1485
OUT
(KIN),A
051F
3A89FF
1486
LD
A,(USERAF+l)	;1stMl•
0522
C3FBFE
1487
1488
1489
JP	TEMPl+l;2ndMl,
;Executethetwoinstructions
;storedinRAM.Theyare:









1503
1504
1505
1506
1507
1508

·,**************************************************************
Themonitorreserves26locationsinmemoryforthetwenty-oneregistersasfollows:
AFBCDEHLAF'BC'DE'HL'IXIYSPPCI




..









LOC

MPF IPOBJCODEMSTMTSOURCESTATEMENT

1983.1.l	PAGE27
ASM5.9

1509
1510
1511
1512
1513
1514
1515

Type<CR>--Displayregisters(twopairsofregisters).
Type<registername><CR>--Displayregisters(pairof
registers)Type	--Alterregistercontents.
REGEXC:
CALLREGEX2CALL
ECHO CH ;Echo the input character with <R>=GET	;Getastringofcharacters
;endthe inputwith<CR>•



;Jump toREGEX2ifone
;oftheinputdatasisillegal.

;SetC=0


;Displaythefirstfourregister
;contents(AFBC).

;Check'condition.
;(i.e.,A'	F'B'C'D'E'H'L')


054B
054C

7EFE3A_

1541
1542
1543
1544
1545

LD
Pl05	CP

A,(HL)3AH


;Check:condition.
;Ifzero,thenchangethecontent
;ofasinglebyteregister
;(i.e.,AFBCDEHLA'F'B'C'D'E'H'L'I)

;Ifzero,thendisplaytwosingle
;byteregister(AFA'F'I).
;oraregisterpair(BCDEHL
;BC'DE'HL').
;oratwobyteregisters.
;(IXIYSPPC)


;Check'condition.
;(i•e.,AF' BC'DE'HL')




Change the contentsof twosinglebyteregister.
(AFAFIIF)•





·-27-

























1599RDSPL0:























05A3
lA
1625
LD
A,(DE)
;Get
thefirstregistercontent.
05A4
CD9A0A
1626
CALL
HEX2


05A7
18
1627
DEC
DE


05A8
lA
1628
LD
A,(DE)
;Get
thesecondregistercontent.
05A9
CD9A0A
1629
CALL
HEX2


05AC
23
1630
INC
HL


05AD
13
1631
INC
DE


05AE
13
1632
INC
DE


05AF
10E2
1633
DJNZ
RDSPLl


0581
3E20
1634
LD
A,20H
;Set
registermode.
0583
3280FF
1635
LD
(TYPEFG),A


0586
C38C08
1636
JP
REG2




1637
1638
·I**************************************************************


1639



1640
;ExecutedwhenUPorDOWNarrowis pressed.


1641
;Executedinregistermodeonly.


1642



1643
RFOR:	;Displaynextfourregistercontents.
0589
3A03FF
1644
LD	A,(RCOUNT)
05BC
3C
1645
INC	A
058D
3C
1646
INC	A
05BE
3C
1647
INC	A
05BF
3C
1648
INC	A
05C0
FE18
1649
CP	24
05C2
2834
1650
JR	Z,RBACKl
05C4
3030
1651
JR	NC,RBACK2
05C6
3203FF
1652
RFORl	LD	(RCOUNT),A
05C9
3AF2FE
1653
LD	A,(STEPFG)	;IfthecontentofSTEPBFis


1654
;zero.


1655
;it meansMPFIP executesSTEP


1656
;orGOcommand.
05CC
A7
1657
AND	A
05CD
2808
1658
JR	Z,RFOR2
05CF
CDB909
1659
CALL	CLEAR
05D2
2A9EFF
1660
LD	HL,(USERPC)
05D5
CD920A
1661
CALL	HEX4
05D8
1805
1662
JR	RFOR3
05DA
3E52
1663
RFOR2	LD	A,52H	;Getpattern'<R>='
05DC
CD6008
1664
CALL	ECHOCH
05DF
218E0B
1665
RFOR3	LD	HL,RTABLE
05E2
3A03FF
1666
LD	A,(RCOUNT)
05E5
85
1667
ADD	A,L
05E6
6F
1668
LD	L,A
05E7
56
1669
LD	D,(HL)
05E8
23
1670
INC	HL
05E9
5E
1671
LD	E,(HL)
05EA
C36B05
1672
JP	RDSPL4


1673
RBACK:	;Displaylastfourregister


1674
;contents.
05ED
3A03FF
1675
LD	A,(RCOUNT)
05F0
FE02
1676
CP	2
05F2
2804
1677
JR	Z,RBACKl
05F4
38D0
1678
JR	C,RFORl
05F6
3D
1679
RBACK2DEC	A
05F7
3D
1680
DEC	A
05F8
3D
1681
RBACKlDEC	A
05F9
3D
1682
DEC	A




05FA
18CA
1683
1684
1685
1686
1687
1688
1689
1690
1691
1692
1693
JR	RFORl
;**************************************************************
;Therearefourkindsofregistermodifymodeasfolloes:
    (1) <R>=H:<onebytedata><CR>
    (2) <R>=H':<onebytedata><CR>
    (3) <R>=HL:< twobytedata><CR>
    (4) <R>=HL':<twobytedata><CR>

Illegalregistername.









Illegalregistername.








1720
1721
1722
1723
1724
1725
1726

·,**************************************************************
;YoucanexaminetheseregisterswhenyouSTEPor GO
;amachinelanguage.
MEMDP2:
0628
062B
062E
0631
CDB9092A9EFFCD920AC36805
1727
1728
1729
1730
1731
CALLLDCALL
MEMDP3JP
CLEARHL,(USERPC)HEX4REGALL
1732
·,**************************************************************
1733

1734
Findbasesoftheregisternameandcontents.
1735
Input:Registername(ASCIIcode)storedinDE.
1736
Output:HL	BaseofRTABLE(i.e.,pointtoregister
1737
namebeginning).
1738
1739
DE	BaseofREGBF(i.e.,pointtoregister
bufferbegining).
1740
C
CountsofregisterinRATBLE.



)BJCODEM STMTSOURCESTATEMENT	ASM5.9



218E0B
cs
AF4F
7A
0619
BE
2027
7BA72808
1741
1742
1743
1744
1745
1746
1747
1748
1749
1750
1751
1752
1753
1754
1755

SEARCREG:
LDPUSHXORLDLDLD
SERCH	CP

JRLDANDJR


HL,RTABLEBC
A
C,A
A,DB,25(HL)

NZ,SERCHlA,E
A
Z,SERCH2







;Comparewiththefirst
;register name.


;Zero,ifitisasingle
;byteregister.

;Comparewiththesecond
;registername.









!-::6
::::3
:::::l

79
FE2748
2008
7DC6086F
79
C608
4F

1767
1768
1769
1770
1771
1772
1773
1774
1775
1776

LDCPLDJRLDADDLDLDADDLD

A,C27HC,B
NZ,SERCH3A,L
A,8
L,A
A;c
A,8
C,A


;CheckI	(A'	F'	B1C'D'E'H'L')









,:66E	C9
1790
1791;
RET
;Zeroif illegal registernameexists.
1792
1793
·I**************************************************************
Function:RefertoREADLN.
1794
C--Get astringofcharacters.
1795
NC-- ResetthecontentofINPTR.
1796

266F
DADA09
1797GET:
1798
JP
C,RDLOOP




0672
CDD409
1799
GETT
CALL
READLN
0675
CD9503
1800

CALL
DECDSP
0678
C9
1801

RET

1802
;


1803
·,**************************************************************


1804
;TAPEWRITE:


1805



1806
DUMP:
0679
CD6008
1807
CALL	ECHOCH	;Echotheinputcharacter


1808
;with<W>=
067C
CD6.F06
1809
DUMPl	CALL	GET	;Get astringofcharacters


1810
;andendtheinputwith<CR>
067F
CDDF08
1811
CALL	CHKHEX	;Getstartingaddress.
0682
38F8
1812
JR	C,DUMPl	;JumptoDUMPliftheinput
0684
22D4FE
1813
1814

LD
;datumareillegal.
(STEPBF+4),HL
0687
CDE508
1815

CALL
GETHL
;Getendingaddress.
068A
38F0
1816

JR
C,DUMPl
;JumptoDUMPliftheinput


1817



;datasareillegal.
068C
22D6FE
1818

LD
(STEPBF+6),HL

068F
CDAE08
1819
DUMP2
CALL
GETCHR
;Gettapefilename.
0692
11D0FE
1820

LD
DE,STEPBF

0695
010400
1821

LD
BC,4

0698
EDB0
1822

LDIR


069A
CD9F07
1823
CALL
SUMl
;Loadparameters from


1824


;stepbufferintoregisters


1825


;Checkiftheparameters


1826


;arelegal.Iflegal,calculate


1827


;thesumofalldatatobe


1828


;outputtotape.
069D
3825
1829
JR
C,ERROR
;BranchtoERRORifthe


1830


;parametersareillegal.(length


1831


;isnegative)
069F
32D8FE
1832
LD
(STEPBF+8),A
;Storetheckecksuminto


1833


;STEPBF+8.
06A2
21A00F
1834
LD
HL,4000
;OutputlKHzsquare


1835


;wavefor4000cycles.


1836


;Leadingsync signal.
06A5
CD6E08
1837
CALL
TONElK

06A8
21D0FE
1838
LD
HL,STEPBF
;Output27bytesstarting


1839


;atSTEPBF.(Include:


1840


;filename,starting,ending


1841


;addressandchecksumandall


1842


;theparametersofEDITORand


1843


;ASSEMBLER.
06AB
011B00
1844
LD
BC,27

06AE
CDBF07
1845
CA[.L
TAPEOUT

06Bl
2lli:00F
1846
LD
HL,4000
;Output2KHzsquare


1847


;wavesfor4000cycles.


1848


;Middlesync.Thefilenameof


1849


;thefilebeingreadwillbe


1850


;displayedintheinterval.
06B4
CD7208
1851
CALL
TONE2K

06B7
CDAC07
1852
CALL
GETPTR
;Loadparametersinto


1853


;registers(Starting,endingand


1854


;length).
06BA
CDBF07
1855
CALL
TAPEOUT
;Outputuser'sdata
06BD
21A00F
1856
LD
HL,4000
;Output4000cycles



06C0
06C3


CD7208C9
1857
1858
1859
1860
18_61;


CALLRET


TONE2K
;of2KHzsquarewave.
;(Tail sync.)








06C4
06C7








21AF0CC38608
1862
1863
1864
1865
1866
1867
1868
1869
1870
1871
1872
1873
1874
1875
1876
1877
1878
·I*************************************************************
Function: Print ERROR message.Input:None
Output: Display patterns ' ERRORS' in display buffer.(OUTPTR)<-INPBF+8
(DISP)	<-DISPBF+l6
Reg affected: AF HLCall:PRTMES
ERROR:
LO	HL,ERRSMSG
JP	PRTMES
;
·I*************************************************************
;Function:TAPEREAD.
LOAD:

06F5
0604
1915
LD
B,4
GetfilenamefromDISPLAYBUFFER.


1916


Thefilenameisconsistedof4


1917


alphanumericcharacters.
06F7
213EFF
1918

LD
HL,DISPBF+l8
06FA
2284FF
1919

LD
(DISP),HL
06FD
21D0FE
1920

LD
HL,STEPBF
0700
7E
1921
LOOP3
LD
A,(HL)
0701
CD2108
1922

CALL
CONVER
0704
23
1923

INC
HL
0705
10F9
1924

DJNZ
LOOP3
0707
0664
1925

LD
B,100	;Displayitfor1.57sec.
0709
CD9B02
1926
FILEDP:
CALL
SCANl
070C
10FB
1927

DJNZ
FILEDP
070E
0604
1928

LD
B,4	;Check·iftheinput


1929


;filenameequalstothe


1930


;specifiedfilenames.
0710
2B
1931

DEC
HL
0711
ED5B82FF
1932

LD
DE,(OUTPTR)
0715
1B
1933

DEC
DE
0716
IA
1934
LOOP4
LD
A,(DE)
0717
BE
1935

CP
(HL)
0718
2B
1936

DEC
HL
0719
1B
1937

DEC
DE
071A
20B4
1938

JR
NZ,LEAD	;Ifnot,findtheleading


1939


;syncofnextfilename.
071C
10F8
1940

DJNZ
LOOP4
071E
3E3F
1941

LD
A,3FH	;Iffilenameisfound


1942


;thendisplay'
0720
D390
1943

OUT
(SEGl),A
0722
3EFF
1944

LD
A,0FFH
0724
D391
1945

OUT
(SEG2),A
0726
CDAC07
1946

CALL
GETPTR	;Theparameters(starting


1947


;endingaddressandchecksum)


1948


;havebeenloadintoSTEPBF.


1949


;Loadthemintoregisters,


1950


;calculatethe blocklength


1951


;andcheckiftheyarelegal.
0729
3899
1952

JR
C,ERROR	;JumptoERRORifinput


1953



;isnotsuccessful.
072B
CD3B07
i954
CALL
TAPEIN
;Inputuser'sdata.
072E
3894
1955
JR
C,ERROR

0730
CD9F07
1956
CALL
SUMl
;Calculatethesumofall
0733
?-1D8FE
1957
LD
HL,STEPBF+8

0736
BE
1958
CP
(HL)
;Compareitwiththe


1959


;checksumcalculatedbyand


1960


;storedby'W'FUNCTION.
0737
C2C406
1961
JP
NZ,ERROR
;JumptoERRORif not


1962


;matched.
073A
C9
·1963
RET




1964



1965
·I**************************************•***********************
1966
TAPEIN:
1967
Loadamemoryqlockfromtape.
1968
Input:HL--startingaddressoftheblock
1969
BC--lengthoftheblock
1970
Output:Carryflag,!--readingerror
1971
0--noerror
1972
Destroyedreg.--AF,BC,DE,HL,AF',BC',DE',HL'




1973















0748	CD5907
074B	1608
074D	CD5907

1982
1983
B84
1985
1986
1987
1988
1989
1990
1991
1992
1993
1994
1995
1996

GETBYTE:
Read one byte from tape.Output:E--dataread
CarryofF',l--readingerror
0 -- no errorDestroyreg.--AF,DE,AF',BC',DE',HL'Byteformat:

startbitbitbitbitbitbitbitbitstopbit	0	1	2	3	4	5	6	7	bit
CALL	GETBIT	;Getstartbit.
LO	D,8	;Loop8times.
BLOOP:CALL	GETBIT	;Getonedatabit
;resultincarryflag.
;RotateitintoE

;Getstopbit.
0758	C9









0759	D9
2001
2002
2003
2004
2005
2006
2007
2008
2009
2010
2011
2012
2013
RET
GETBIT:
Readonebitfromtape.
Output:Carryof F,0	thisbit is0
1	thisbitis1CarryofF',l	readingerror
0	noerrorDestroyed reg. -- AF,AF',BC',DE',HL'Bitformat:

0--2KHz8cycles+lKHz2cycles.1--2KHz4cycles+lKHz4cycles.
EXX











2029
2030
Bit 0 of H register is used to indicate the usageofahighfreqperiod.Ifthisbitiszero,high






2031
freqperiodcausescounterincrementforthecurrent

2032
tape-bit.Ifthehighfreqpart haspassed,bit0

2033
ofHissetandihenexthighfreqperiodwillbeused

2034
asaterminator.

2035
Lregisterisusedtoup/downcountthenumberofperiods.

2036
whenahighfreqperiodisread,·Lis	increasedby

2037
l;when alowfreqperiodisread,Lisdecreased

2038
by2.(Thetimedurationforeachcountis0.5ms.)

2039
Attheendofatape-bit, positiveandnegativeL

2040
standfor0andlrespectively.

2041

075A
210000
2042
LD	HL,00
075D
CD7A07
2043
COUNT!:CALL	PERIOD	;Readoneperiod.
0760
14
2044
INC	D	;Thenexttwoinstruc.tions


2045
;checkifDiszero.Carryflag


2046
;isnot affected.
0761
15
2047
DEC
D
0762
2011
2048

JR
NZ,TERR
;IfDisnotzero,jump


2049



;toerrorroutineTERR.


2050



;(Becausetheperiodistoo


2051



;muchlongerthanthatoflKHz).
0764
3806
2052

JR
C,SHORTP
;Ifthe periodisshort


2053



;(2KHz),jumptoSHORTP.
0766
2D
2054

DEC
L
;TheperiodislKHz,


2055



;decreaseLby2•Andset


2056



;bit0 ofHtoindicatethis


2057



;tape-bithaspassedhighfreq


2058



;partandreachesits·lowfreq


2059



;part.
,0767
2D
2060

DEC
L

0768
CBC4
2061

SET
0,H

076A
18Fl
2062

JR
COUNTl

076C
2C
2063
SHORTP:
INC
L
;Theperiodis 2KHz,


2064



;increaseL by1.
076D
CB44
;1065

BIT
0,H
;Ifthetapebithaspassed


2066



;itshighfreqparthigh


2067



;frequencymeansthisbitisall


2068



;overandnextbithasstarted.
076F
?SEC
2069

JR
Z,COUNTl



2070


;L=(#of
2Kperiod)-2*(#oflKperiod)
0771
CB15
2071

RL
L



2072



0--NCarry(Lpositive)


2073


;
1---Catry(Lnegative)
2074
;Thepositive ornegativesign of
2075
;Lcorrespondstothetape-bitdata.
2076
;'RLL'willshiftthesignbitof
2077
;Lintocarryflag.Afterthis
2078
;instruction,thecarryflag


2079


;containsthetape-bit.
0773
D9
2080

EXX

;RestoreBC'DE'HL'
0774
C9
2081

RET


0775
08
2082
TERR:
EX
AF,AF'

vf776
37
2083

SCF

;SetcarryflagofF'toindicateerror.
0777
08
2084

EX
AF,AF'

0778
D9
2085

EXX


0779
C9
2086

RET




2087
PERIOD:



2088;Waitthetapetopassoneperiod.







MPFIP
1983.1.1
PAGE37
LDC
OBJCODE
M
STMT
SOURCESTATEMENT

ASM5.9

















0795
20F9
2110

JR
NZ,LOOPL
;Loopuntilinputgoeshigh.
0797
3EFF
2111

LD
A,111111118
;Echothetapeinputto


2112



;speakeronMPFIP.
0799
D392
2113

OUT
(KIN),A

079B
7B
2114

LD
A,E

079C
FE20
2115

CP
MPERIOD
;Comparetheresultwith


2116



;thethreshold.
079E
C9
2117

RET




2118
;





2119
·,**************************************************************


2120
SUM!:


2121
Calculatethesum ofthe datainamemory


2122
block.Thestartingandendingaddress


2123
ofthisblockarestoredinSTEPBF+2-STEPBF+4.


2124
RegistersAF,BC,DE,HLaredestroyed.


2125

079F
CDAC07
2126
CALL	GETPTR	;Getparametersfrom


2127
;stepbuffer.
07A2
D8
2128
RET	C	;Returnifthe parameters


2129
;areillegal.


2130
SUM:


2131
Calculatethesumofamemoryblock.


2132
HLcontainsthestartingaddressof


2133
thisblock,BCcontainsthelength.


2134
TheresultisstoredinA.Registers


2135
AF,BC,HLaredestroyed.


2136

07A3
AF
2137
XOR
A
;ClearA
07A4
86
2138
SUMCAL:ADD
A,(HL)

07A5
EDAl
2139

CPI


07A7
EAA407
2140

JP
PE,SUMCAL

07AA
B7
2141

OR
A
;Clearflag.
07AB
C9
2142

RET




2143
GETPTR:



2144
Getparametersfromstepbuffer.
2145
Input:(STEPBF+4)and(STEPBF+S)contain
2146
startingaddress.
LOC
OBJCODEMSTMTSOURCESTATEMENT
ASM5.9


2147
(STEPBF+6)and(STEPBF+7)contain

2148
endingaddress.

2149
Output:HLregistercontainsthestarting

2150
address.

2151
BCregistercontainsthelength.

2152
Carryflay0.--BCpositive

2153
1--BCnegative

2154
Destroyedreg.:AF,BC,DE,HL.

2155

07AC
21D4FE
2156
LD	HL,STEPBF+4
07AF
5E
2157
GETP
LD
E,(HL)
;Load
thestartingaddress


2158



;into
DE •
07B0
23
2159

INC
HL


07Bl
56
2160

LD
D,(HL)


07B2
23
2161

INC
HL


07B3
4E
2162

LD
C,(HL)


07B4
23
2163

INC
HL


07B5
66
2164

LD
H,(HL)
;Load
endingaddress


2165



;into
HL.
07B6
69
2166

LD
L,C


07B7
B7
2167
OR
A
;Clearcarryflag.
07B8
ED52
2168
SBC
HL,DE
;Finddifference.


2169


;Carryflagischangedhere.
07BA
4D
2170
LD
C,L

07BB
44
2171
LD
B,H

07BC
03
2172
INC
BC
;NowBCcontainsthe


2173


;length.
07BD
EB
2174
EX
DE,HL
;NowHLcontainsthe


2175


;startingaddress.
07BE
C9
2176
RET




2177





2178
;*********•****************************************************


2179
TAPEOUT:


2180
Outputamemoryblocktotape.


2181
Input:HL--startingaddress oftheblock


2182
BC--lengthoftheblock


2183
Destroyedreg.--AF,BC,DE,HL,BC',DE',HL'


2184

07BF
SE
2185
LD	E,(HL)	;Getthedata.
07C0
CDC907
2186
CALL	OUTBYTE	;Outputtotape.
07C3
EDAl
2187
CPI
07C5
EABF07
2188
JP	PE,TAPEOUT	;Loopuntilfinished.
07C8
C9
2189
RET


2190
OUTBYTE:


2191
Outputonebytetotape.Fortape-byte


2192
format,seecommentsonGETBYTE.


2193
Input:E--data


2194
Destroyedreg.--AF,DE,BC',DE',HL'


2195

07C9
1608
2196
LD	D,8	;Loop8times.
07CB
B7
2197
OR	A	;Clearcarryflag.
07CC
CDDC07
2198
CALL	.OUTBIT	;Outputstartbit.
07CF
CBlB
2199
OLOOP:RR	E	;Rotatedataintocarry.
07Dl
CDDC07
2200
CALL	OUTBIT	;Outputthe carry.
07D4
15
2201
DEC	D
07D5
2.0F8
2202
JR	NZ,OLOOP
07D7
37
2203
SCF	;Setcarryflag.
07D8
CDDC07
2204
CALL	OUTBIT	;Outputstopbit.
LOC
OBJCODEMSTMTSOURCESTATEMENT
ASM5,9





07DC
D9
2210
EXX
07.DD
2600
2211
LD
H,0
07DF
3809
2212

JR
C,OUTl
;Ifdata
l,output1•
07El
2E08
2213
OUT0:
LD
L,ZERO2K


07E3
CD7208
2214

CALL
TONE2K­


07E6
2E02
2215

LD
L,ZEROlK


07EB
1807
2216
2217

OUTl:
JR
BITEND-

;2K4cycles,lK4cycles
07EA
2E04
2218
LD
L,ONE2K

07EC
CD7208
2219
CALL
TONE2K

07EF
2E04
2220
LD
L,ONElK

07Fl
CD6E08
2221
BITEND:CALL
TONElK

07F4
D9
2222
EXX

;Restoreegisters.
07F5
C9
2223
RET


2224
2225
2226
2227
2228
2229
2230
2231
2232
2233
2234
2235
;'**************************************************************
Function:Cleardisplaybufferanddispliyprompt.Input:None
Output:(OUTPTR)<-INPBF
(DISP)	<-DISPBF
IX<-DISPBF
Setallthecontents ofdisplaybuffertobeFF.
Regaffected:AFIXCall:CLEARCHRWR.

CLRBF:
07F6
07F9
07FB
07FE
0802
CDB9093E3CCD2409DD212CFFC9
2236
2237
2238
2239
2240
2241'
CALLLDCALLLDRET
CLEARA,3CHCHRWRIX,DISPBF
2242
2243
2244
2245
·**************************************************************
Function:Generateasound.Input:None
Output:None










0819	7E
081A	2F
081B	77
081C	7E
0810	2F
081E	77
081F	BE
0820	C9
2263
2264
2265
2266
2267
2268
2269
2270
2271
2272
2273
2274
2275
2276
2277
2278
2279
2280
2281
2282
2283
2284
2285
2286
2287
2288
;
·I**************************************************************
Function:checkifa memoryaddressisinRAM.
Input:HL--addresstobe check.
Output:Zeroflag--0,ROMornonexistant;
1,RAM.
Destroyedreg.:AF.Call:none

RAMCHK:
LD	A,(HL)CPL
LD	(HL),A
LD	A,(HL)CPL
LO	(HL),A
CP	(HL)RET
;
·,**************************************************************
Function:Convertabyte(ASCIIcode)inAregistertodisplaypattern.
Input:A--ASCIIcode.
(DISP)--Pointtotheresultaddressin displaybuffer.
Output:Patternfortwobytes.Thefirstbytein(DISP)andthesecondbytein(DISP)+l•







































086E
0870
0872










0E41
1802
0ElF

2359
2360
2361
2362
2363
2364
2365
2366
2367
2368
2369
2370
2371
2372
2373
2374

,
·,**************************************************************
Function: Generate square wave to the MIC&speakeronMPFIP.
Input:C--period;2*(44+13*C)clockstates.
HL--numberofperiods.
Output: none.
Destroyedreg.:AF,B(C),DE,HL.Call:none.
TONElK:
LO	C,FlKHZ
JR	TONE
TONE2K:
LO	C,F2KHZ
TONE:







































086E
0870
0872










0E41
1802
0ElF

2359
2360
2361
2362
2363
2364
2365
2366
2367
2368
2369
2370
2371
2372
2373
2374

,
·,**************************************************************
Function: Generate square wave to the MIC&speakeronMPFIP.
Input:C--period;2*(44+13*C)clockstates.
HL--numberofperiods.
Output: none.
Destroyedreg.:AF,B(C),DE,HL.Call:none.
TONElK:
LO	C,FlKHZ
JR	TONE
TONE2K:
LO	C,F2KHZ
TONE:

087C
41
2379
LD
B,C
087D
10FE
2380
DJNZ
$
087F
EE20
2381
XOR
20H	;TOGGJ:.,EOUTPUT
0881
ED52
2382
SBC
HL,DE
0883
20F5
2383
JR
NZ,SQWAVE
0885	C9	2384
2385
RET
,
2386
;**************************************************************
2387
Function:Printmessageuntil<CR>met.
2388
Input:HL--Startingaddressofcharacters.
2389
Output:(OUTPTR)<-(OUTPTR)+?
2390
(DISP)	<-(DISP)+2*?
2391
?isthenumberofcharcterstobeprinted.
2392
2*? isfailsifthereexistsTABkeyininputbuffer.
2393
Regaffected:AFHL
2394
Call:CLEARMSGDECDSPCR2•
2395



2396
PRTMES:

0886
CDB909
2397

CALL
CLEAR
0889
CDCA09
2398

CALL
MSG
088C
CD9503
2399
REG2
CALL
DECDSP
088F
CD8109
2400

CALL
CR2
0892
C9
2401

RET



2402
;





2403
·,*************************************************************



2404
Function:Printoutallthecontentsindisplaybuffer.



2405
Input:None



2406
Output:None



2407
Regaffected:AF



2408
Call:PTESTMTPPRT



2409

0893
CDA308

2410
PRINTTCALL	PTEST
0896
C0

2411
RET	NZ
0897
DDE5

2412
PUSH	IX
0899
DD2104FF

2413
LD	IX,INPBF
089D
CD0000
X
2414
CALL	MTPPRT	Refertoprintermanual.
08A0
DDEl

2415
POP·	IX
08A2
C9

2416
RET
2417
,
2418
;**************************************************************
2419
Function:Checkthetoggleprinterswitchand
2420
theconditionofprinterinterface.
2421
Input:None
2422
Output:Zero
flag=
1
(1)Printerexistsandtoggle
2423



switchison.
2424




2425
Zero
flag
0
(2) Printerexistsbutthe
2426



toggleswitchis off.
2427



(3)Printernotexists.
2428
Regaffected:
AF


2429
Call:None



2430





08A8
08AB
08AD
3A0060FECD
cg
2437
2438
2439
2440I
LD	A,(6000H)
CP	0CDHRET










08AE










2A7EFF
2441
2442
2443
2444
2445
2446
2447
2448
2449
2450
2451
2452
2453
2454
·,**************************************************************
Function: Use (GETPT) as a pointer increase HL until(HL-1)isoneofthefollowingdelimeters:
SPACETAB•:/=	and(HL+l)isnotSPACEorTAB.
Input: HL=(GETPT) -- Starting address.Output:HL<-HL+?
(GETPT)<-(GETPT)+?
Reg affected: AF HL.Call:None
GETCHR:
LD	HL,(GETPT)
LDA:
0881
0882
0884
0886
0888
08BA
0888
7EFE202804FE09200E
23
7E
2455
2456
2457
2458
2459
2460
2461
2462




SKIP:





STPTR:
LDCPJRCPJR

INCLDCPJRCPJR
A,(HL)
I	I

Z,SKIP_
NZ,EOS?HL
A,(HL)
I	I
Z,SKIP_Z,SKIP_

;SPACE••
;TAB.



;SPACE.
;TAB.

EOS?:
LD	(GETPT),HLRET










CHKHEX:
CPJRCPJRCPJRCPJRCPJRINCJR
0DHZ,STPTR3AHZ,SKIP2EH-Z,SKIP3DH-Z,SKIP2FH-Z,SKIPHL	-
LDA
;Endofstring?
;Yes
;.
;=
;/

CHKHE2:
,
LD	HL,INPBF
LD	(GETPT),HL
·,**************************************************************
Function:CallGETCHRandconvertASCIIcodestohexadecimalvaluesandstorethemintoHL.
Input:(GETPT)
Output:(GETPT)<-(GETPT)+?
A<-L
H=0	Ifthereisonlyone hexadecimaldigit.

2495
Carryflag=lIf
thedataisnothexadecimaldigits.
2496
Zeroflag=1If
thelastASCIIcodeis<CR>•
2497
Regaffected:AFDEHL

2498
Call:
GETCHRONE•


2499






2500
GETHL:


;Get4digit numbertoHL&L=A


2501



;C(Nonhexadecimalvalues)


2502



;Z(0DH)
08E5
210000
2503

LD
HL,0
;Assumeinput0000
08E8
ES
2504

PUSH
HL
;Temporarystorein(SP),(SP+l)
08E9
39
2505

ADD
HL,SP
;HL=SP
08EA
EB
2506

EX
DE,HL
;BorrowSPfortemporybuffer.
08EB
CDAE08
2507

CALL
GETCHR

08EE
EB
2508

EX
DE,HL

08EF
FE30
2509
CV3
CP
'0'
08Fl
300A
2510

JR
NC,CVT
08F3
FE0D
2511

CP
0DH
08F5
2003
2512

JR
NZ,CV2

08F7
El
2513
CVl
POP
HL

08F8
7D
2514

LD
A,L
;Stringend.
08F9
C9
2515

RET




2516
CV2:



08FA
A7
2517
AND
A
08FB	18FA	2518	JR
CVl


2519
CVT:



08FD
FE3A
2520
CP
3AH
08FF
28F9
2521
2522

CVTHEX:
JR
z,cv2

0901
CD140B
2523
CALL
ONE
;ASCIItoHEX
0904
380A
2524
JR
C,NOTHEX

0906
ED6F
2525

RLD

;Rotateinto(HL)i.e.(SP)
0908
23
2526.
INC
HL
;SP+l
0909
090B
090C
090D
090E
0910
0911
ED6F2B
13
lA
18DF

ElC9
2527
2528
2529
2530
2531
2532
2533
2534
2535




NOTHEX:

,
RLDDECINCLDJR

POPRET

HLDE
A,(DE)CV3
HL




;Error
2536
2537
2538
2539
·,**************************************************************
Function: Check the numbers of content in display buffer,ifitexcess40thechangethe IXpointer.
Input:(DISP)
2540
2541
Output:IX<-IX
(Ifthe numberofcontentsarelessthan40).
2542
2543
IX<-(DISP)-38(Ifthenumbersofcontentsarelagerthan40)•
2544
2545
2546
2547
Carryflag=1Reg affected: AF DEHLCall:None
If(DISP)<(DISP)+38
IX.

0918
DD212CFF
2553

LO
IX,DISPBF
091F
D8
2554

RET
C
0920
EB
2555

EX
DE,HL
0921
0D19
2556

ADD
IX,DE
0923
C9
2557

RET



2558
;


2559
2560
2561
2562
2563
2564
2565
2566
2567'
2568
2569
2570
2571
2572
2573
2574
·,**************************************************************
Function: Convert a byte (ASC II code) in A registertodisplaypatternsandstoretheminto
inputbufferanddisplaybufferrespectively.
Input:A--abyteofASCIIcode.
(OUTPTR) -- Point to the result address in input buffer.(DISP)-- Pointto theresultaddressindisplaybuffer.
Output:StoretheASCIIcodeinto(OUTPTR)
Patternfortwobytes.Thefirstbytein(DISP)andthesecondbytein(DISP)+l•
(OUTPTR)<-(OUTPTR)+l(DISP)	<-(DISP)+2
Regaffected:AFCall:CONVERCURSOR
CHRWR:
0924
ES
2575
PUSH
HL
0925
D5
2576
PUSH
DE
0926
2A82FF
2577
LD
HL,(OUTPTR)
0929
77
2578
LD
(HL),A
092A
23
2579
INC
HL
092B
2282FF
2580
LO
(OUTPTR),HL
092E
FE09
2581
CP
9
0930
2857
2582
JR
Z,TABOUT
0932
CD2108
2583
CALL
CONVER


2584
TABRET:

0935
CD790A
2585
CALL
CURSOR
0938
Dl
2586
POP
DE
0939
El
2587
POP
HL
093A
C9
2588
RET












093B











3E05
2589
2590
2591
2592
2593
2594
2595
2596
2597
2598
2599
2600
2601
2602
2603
2604
;
·,**************************************************************
;Function:Print out all the contents in input bufferChecktheTV interface,ifTVinterfaceboardexiststhenjumptoTVinterfaceserviceroutine.
TherearefourkindsofCRXasfollows:
Input:(OUTPTR)--Pointtotheresultaddressininputbuffer.
Output:(OUTPTR)<-INPBF
(DISP)	<-DISPBF
Regaffected:AF.
Call:CR0PTESTPRINTTCLEARCURSOR.
CR:
LO	A,5
CR4:














interface.



2633
2634
2635
2636
2637
2638
2639
2640
2641
2642

;'**************************************************************
Function:SameasCRbut thedisplaytimingisaboutlsec.Input:(OUTPTR)--Pointtotheresultaddressininputbuffer.
Output:(OUTPTR)<-INPBF
(DISP)	<-DISPBF
Regaffected:AFAF'BC'DE'HL',HL•Call:CR0PTESTSCANlPRINTTCLEARCURSOR
CRl:









0983
18B8
2656
2657;
JR	CR4
2658
2659
2660
2661
2662
2663
2664
2665
2666
;**************************************************************Function: Same as CR but CR3 call routine CLRBF insted of CLEARInput:(OUTPTR)--Pointtotheresultaddressininputbuffer.
Output: (OUTPTR)<-(OUTPTR)+l(DISP)	<-(DISP)+2
Regaffected:AFIX.Call:CR0PTESTCLRBF.
CR3:
0985
3E30
2667
LD
A,30H
0987
1884
2668
JR
CR4




2669














2689
2690
2691

TAB?:

;CheckifcursoratTABposition.
;Zero flag:Setifyes.




2698TAB?LP:



2704
2705
2706
2707
2708
2709
2710
2711
·I**************************************************************
Function: Clear  the display  buffer and set the contentsof(DISP) and(OUTPTR)tothestartingaddressof displaybuffer andinputbufferrespectively.
Input:None
Output:(OUTPTR)<-INPBF
(DISP)	<-DISPBF
SetallthecontentsofdisplaybuffertobeFF.








2723
2724
2725
2726
;
;**************************************************************
Function:ConvertASCIIcodestodisplaypatternsuntil
<CR>met.
















09CA
09CB
09CC
09CE
09CF
09D2











7E
23
FE0DCBCD240918F6
2727
2728
2729
2730
2731
2732
2733
2734
2735
2736
2737
2738
2739
2740
2741
2742
2743
2744
2745
2746
2747
2748
2749
2750
2751
2752
2753
2754
2755
2756
2757
2758
2759
2760
2761
2762
2763
Use HL as a pointer , convert the ASCII codes todisplaypatternsandstoredthemintodisplaybuffer.
Input:HL--Startingaddressof characters.
(OUTPTR)--Pointtotheresultaddressininputbuffer.(DISP)--Pointtotheresultaddressindisplaybuffer.
Output:HL<-HL+?
(OUTPTR)<-(OUTPTR)+?(DISP)	<-(DISP)+2*?
?isthenumberof cheracterstobeprinted.
Regaffected:AFHL.Call:CHRWR

MSG:
LD	A,(HL)
INC	HL
CP	0DH
RET	z
CALL	CHRWR
JR	MSG
·,**************************************************************Function: Getastringofcharactersandendwith<CR>•Input:
(OUTPTR)--Pointtotheresultaddressininputbuffer.(DISP)--Pointtotheresultaddressindisplaybuffer.
Output:(INPTR)<-(OUTPTR)(OUTPTR)<-(OUTPTR)+?(DISP)	<-(DISP)+2*?
?isthenumber ofinputcharacters.Iftheinputcharacters containsTABcode,thencondition2*?fails.(COUNT)--Numberofcharactersincluding<CR>
zeroflag--Setifonly<CR>isdepressed.
Regaffected:AFBCDEHLAF'BC'DE'HL'•Call:CHK40CURSORCR0SACNCHRWR
READLN:
09D4	2A82FF
09D7	2286FF
2764
2765
LD	HL,(OUTPTR)
LD	(INPTR),HL

;Setinputpointer.

2766
RDLOOP:

09DA
CD1209
2767

CALL
CHK40	;Adjust
IXpointer.
09DD
CD790A
2768

CALL
CURSOR

09E0
3E50
2769

LD
A,50H

09E2
3281FF
2770

LD
(CRSET),A

09E5
CD71M
2771

CALL
CR0	;Check
TVinterface.
09E8
CD4602
2772

CALL
SCAN

09EB
FEll
2773

CP
llH

09ED
CAE600
2774

JP
Z,ESCAPE
;SOFTWAREESCAPE(CONTROLQ).
09F0
FE0D
2775

CP
0DH	CR

09F2
2822
2776

JR
Z,RD_END

09F4
FE5F
2777

CP
05FH	<--

09F6
282E
2778

JR
Z,LEFT

09F8
FE5E
2779

CP
5EH
;UParrow.
09FA
28DE
2780

JR
Z,RDLOOP

09FC
FE69
2781

CP
69H
;DOWNarrow.
09FE
28DA
2782

JR
Z,RDLOOP

0A00
2A84FF
2783

LD
HL,(DISP)

0A03
ll7CFF
2784

LD
DE,DISPBF+80
;Checkthenumbersofcharacter





2785


ininputbuffer.

2786


The numbersofinputcharacters

2787


islimitedto40.
0A06
A7
2788
AND
A

0A07
ED52
2789
SBC
HL,DE

0A09
30CF
2790
JR
NC,RDLOOP

0A0B
FE68
2791
CP
KTAB
;CheckTABkey
0A0D
2002
2792
JR
NZ,NOTTAB
0A0F
3E09
2793

LD
A,09
;09istheASCIIcodefor


2794



;TABkey.


2795
NOTTAB:



0All
CD2409
2796

CALL
CHRWR

0Al4
18C4
2797

JR
RDLOOP



2798
RD
END:


0Al6
2A82FF
2799


LD
HL,(OUTPTR)
0Al9
77

2800
LD
(HL),A
Store0DH
0AlA
ED5B86FF

2801
LD
DE,(INPTR)

0AlE
ED52

2802
SBC
HL,DE
zeroflag
0A20
23

2803
INC
HL

0A21
7D

2804
LD
A,L

0A22
320000
X
2805
LD
(COUNT),A
;Set/COUNT/
0A25
C9
2806
RET


2807
LEFT:


;Backspacekeyserviceroutine.
0A26
2A86FF
2808

LD
HL,(INPTR)

0A29
ED5B82FF
2809

LD
DE,(OUTPTR)

0A2D
A7
2810

AND
A

0A2E
ED52
2811

SBC
HL,DE

0A30
30A8
2812

JR
NC,RDLOOP
;IgnoreifexceedingLEFTend.
0A32
EB
2813

EX
HL,DE

0A33
2B
2814

DEC
HL
;Decreasethepointerof


2815



;inputbufferbyone.
0A34
2282FF
2816

LD
(OUTPTR),HL

0A37
7E
2817

LD
A,(HL)

0A38
FE09
2818

CP
09

0A3A
2805
2819

JR
Z,B TAB

0A3C
CD6A0A
2820

CALL
BSP

0A3F	1899	2821
JR
RDLOOP
2822BTAB:
0A41
CD6A0A
2823

CALL
BSP
0A44
CDA909
2824

CALL
TAB?	;CheckifcursoratTABposition.
0A47
280A
2825

JR
Z,BTABl
0A49
2A84FF
2826

LD
HL,(DISP)
0A4C
2B
2827

DEC
HL
0A4D
7E
2828

LD
A,(HL)
0A4E
2B
2829

DEC
HL
0A4F
A6
2830

AND
(HL)
0A50
3C
2831

INC
A
0A51
28EE
2832

JR
Z,B_TAB


2833
BTABl:


0A53
2A82FF
2834

LD
HL,(OUTPTR)


2835
BTAB2:


0A56
2B
2836

DEC
HL
0A57
7E
2837

LD
A,(HL)·
0A58
0A5A
FE20
C2DA09
2838
2839
CP
JP
I	I

NZ,RDLOOP
0A5D
CDA909
2840
CALL
TAB?
0A60
CADA09
2841
JP
Z,RDLOOP
0A63
3E20
2842
LD
A,'I




0A65
CD2108
2843
CALL
CONVER
0A68
18EC
2844
JR
B TAB2


2845
BSP:


;Cleartherightmostpatterns


2846



;indisplaybuffer.
0A6A
E5
2847

PUSH
HL

0A6B
CD9903
2848

CALL
DEC SP

0A6E
2B
2849

DEC
HL

0A6F
28
2850

DEC
HL

0A70
2B
2851

DEC
HL

0A71
2284FF
2852

LD
(DISP),HL

0A74
CD790A
2853

CALL
CURSOR

0A77
El
2854

POP
HL

0A78
C9
2855

RET


2856
2857
2858
2859
2860
2861
2862
2863
2864
2865
2866

I
·I**************************************************************
Function:Getcursormessage.
Input:(DISP)--Pointtotheresultaddressindisplaybuffer.
Output:Thefirstbyteof cursorin(DISP)andthesecondbyteofcusorin(DISP)+l
(DISP)<-(DISP)	The contentof(DISP)isunchanged.
Regaffected:AFCall:CONVER

CURSOR:
3A79	3E5B
0A7B	CD2108
0A7E	E5
0A7F	2A84FF
0A82	2B
0A83	2B
0A84	2284FF
0A87	El
0A88	C9
2867
2868
2869
2870
2871
2872
2873
2874
2875
2876
2877

CCURSOR:






;
LD

CALLPUSHLDDECDECLDPOPRET
A,05BH

CONVERHL
HL,(DISP)HL
HL(DISP),HLHL
PROMPT
CALLHEREIFCHANGEPROMPT
2878
2879
2880
2881
2882
2883
2884
2885
2886
2887
2888
2889
2890
2891
;**************************************************************
Function:ConvertbinarydatainHLtoASCIIcodeanddisplaypatterns.
Input:HL--Twobytesof hexadecimalvaluesinHL.
(OUTPTR)--Pointtotheresultaddressininputbuffer.(DISP)--Pointtotheresultaddressindisplaybuffer.
Output:FourASCIIcodein(OUTPTR)-(OUTPTR)+3
Eightbytesofdisplaypatternin(DISP)-(DISP)+7(OUTPTR)<-(OUTPTR)+4
(DISP)	<-(DISP)+8
Regaffected:AF
Call:HEX2
HEXX:
0A8'
0A8A
0A8D
0A8E
0A91
7CCD9A0A7DCD9A0AC9
2892
2893
2894
2895
2896
2897;
LDCALLLDCALLRET
A,HHEX2A,LHEX2
2898
2899
2900
·I**************************************************************
Function:ConvertbinarydatasinHLtoASCIIcodesanddisplaypatterns.

2901
2902
2903
2904
2905
2906
2907
2908
2909
2910
CallroutintSPACEltoinsertaspace.
Input:SameasHEXX
Output:FiveASCIIcodesin(OUTPTR)-(OUTPTR)+4
Ten bytes of display pattern in (DISP) -(DISP)+8(OUTPTR)<-(OUTPTR)+5
(DISP)	<-(DISPJ+l0
Regaffected:AFCall:HEXXSPACE!•

HEX4:
0A92
0A95
0A97
CD890A3E20C32409
2911
2912
2913
2914
CALLSPACE!LD
JP
i
HEXX
A,'	I
CHRWR












0A9A	E5
2915
2916
2917
2918
2919
2920
2921
2922
2923
2924
2925
2926
2927
2928
2929
2930
2931
·,**************************************************************
Function:ConvertbinarydatatoASCIIcodeanddisplaypatterns.
Input:A--abyteinA register.
(OUTPTR) -- Point to the result address in input buffer.(DISP)--Pointtotheresultaddressindisplaybuffer.
Output:ThefirstASCII codein(OUTPTR)andthesecondASCII codein(OUTPTR)+l•Displaypatternsforfourbytes•Thefirstbytein(DISP)andthesecondbytein(DISP)+l,andsoon.
(OUTPTR)<-(OUTPTR)+2(DISP)	<-(DISP)+4
Regaffected:AF
·;Call:HEX!
HEX2:
PUSH	HL





0AA8
0AAB
0AAC

CDAD0A
ElC9

2939
2940
2941
2942i

CALL	HEXl
POP	HL
RET
2943
·,**************************************************************
2944
2945
2946
2947
2948
Function:ConvertbinarydatatoASCIIcodeanddisplaypattern.
Input:A--LSB4 bitscontainsthebinarydata.
(OUTPTR) -- Point to the result address in input buffer.(DISP)--Point totheresultaddressindisplaybuffer.







0AAD







C630
2949·
2950
2951
2952
2953
2954
2955
2956
2957
2958
Output:ASCIIcodein(OUTPTR).
Patternfortwobytes.Thefirstbytein(DISP)andthesecondbytein(DISP)+l•
(OUTPTR)<-(OUTPTR)+l(DISP)	<-(DISP)+2
Regaffected:AFCall: CHRWR
HEXl:
ADD	A,'0'

0AAF
FE3A
2959

CP
'9'+1
0AB1
3802
2960

JR
C,HHH
0AB3
C607
2961

ADD
A,7


2962
HHH:


0AB5
C32409
2963
2964,
JP	CHRWR
2965
2966
2967
2968
2969
2970
2971
2972
2973
2974
2975
2976
2977
·,**************************************************************Function: ConverthexadecimalvaluesinHLtocorrespondingdecimalformat(inASCII CODEformat).
Input:HL--Hexadecimalvaluestobechanged.
(OUTPTR)--Pointtotheresultaddressininputbuffer.(DISP)--Pointtotheresultaddressindisplaybuffer.
Output: (OUTPTR)<-(OUTPTR)+?(DISP)	<-(DISP)+2*?
Regaffected:AFBCDEHLIY•Call:CHRWR

DECIMAL:
0AB8
0ABC
0ABE

0AC0
0AC3
0AC5
0AC8
0ACA
0ACB
0ACD
0ACF
0AD0
FD21A90C060.3
0E00

FD5E00FD23FD5600FD23AF

ED523803
3C
}.8F9
2978
2979
2980
2981
2982
2983
2984
2985
2986
2987
2988
2989
2990
2991


CLOOP:




DECLOOP:
LDLDLD

LDINCLDINCXOR

SBCJRINCJR
IY,TENSB,3
C,0

E,(IY}IY
D,(IY}IY
A

HL,DEC,ADDBACKA
DECLOOP
Tableoftn•spowers.Outputthreedigits.
Zerosupressflag.

0AD2
0AD3
0AD6
0AD8
0AD9
0ADA

0ADC
0ADD
0ADF
0AE2
0AE3

19
CDD90A10E8
cg

A72806

4FC630C32409

79
A7
2992
2993
2994
2995
2996
2997
2998
2999
3000
3001
3002
3003
3004
3005
3006
ADDBACK:



SUPRESS:





YES0:

ADDCALLDJNZRET

ANDJR

LDADDJP
LDAND

HL,DESUPRESSCLOOP

A
Z,YES_0

C,AA,30HCHRWR

A,C
A






;Ifzerothenckeckzero
;supressflag.
Else
ConverttoASCIIcodeformatandoutput.
0AE4
2805
3007
3008
JR
PRINT0:
Z,BLANK0	Supressleadingzero•

3017
3018
3019
3020
3021
3022
3023
3024
3025
3026
3027
3028
3029
3030

·'**********************************'****************************
Function:ConvertASCIIcodestocorrespondinghexadecimal
valuesuntiLmetnonehexadecimaldigit,ThereturnvalueisstoredinHL.
Input:DE--PointtothefirstlocationofASCIIcodetobe changed.
Output:HL--Returnvalues(hexadecimaldigits).
(HEXFLAG)issetifthereexistsadigitwithin('A'••'F')orthelastnonehexadecimalcharacteris'H'
Regaffected:AFBCDEHL.Call:ONE

3031HEXBIN:
3032

0AF4
AF

3033
XOR
A
0AF5
320000
X
3034
LD
(HEXFLAG),A
0AF8
47

3035
LD
B,A
0AF9
0AFA
67
6F

3036	LD
3037	LD
3038HBLOOP:
H,A
L,A


[HL]=l6*[HL]






0B0A
lA
3051
LD
A,(DE)
0B0B
FE48
3052
CP
'H'
0B0D
C0
3053
RET
NZ
0B0E
13
3054
INC
DE
0B0F
320000
X3055
LD
(HEXFLAG),A
0B12
lA
3056
LD
A,(DE)
0B13
C9
3057
RET



3058




3059


3060
3061
3062
3063
3064
;
;**************************************************************
Function:Convertabyte(ASCIIcode)inAregistertohexadecimaldigit.
Input:A--ASCIIcode.
30Vi
3066
Output:A--Hexadecimal
Carryflag=1
values.
Ifthedataisnotahexadecimaldigit.
3067
3068
3069
3070
3071
3072
(HEXFLAG)isnotzeroIfthecontentofAwithin'A'and'F'.
Regaffected:AFCall:None

ONE:
0B14
FE47
3073
CP
'F'+l
0B16
3F
3074
CCF









0BlE
D0

3080
RET
NC
0BlF
D607

3081
SUB
7
0B21
FE0A

3082
CP
10
0B23
D8

3083
RET
C
0B24
320000
X
3084
LD
(HEXFLAG),A
0B27
C9

3085
RET



3086
3087
·,'**************************************************************

3088
Function:ConvertASCIIcodestocorrespondingdecimalvalues

3089
inbinaryuntilmetnondecimaldigits.

3090
Input:DE--PointtothefirstASCIIcode(Decimalformat)

3091
tobechanged.

3092
Output:HL--Returnvalues(Decimaldigits).

3093
Regaffected:AFBCDEHL•

3094
Call:None

3095


3096
DECBIN:

3097

0B28
210000
3098
LD	HL,0
0B2B
lA
3099
LD	A,(DE)
3100
NDIGIT:



[HL)=10*[HL)








3117
3118
3119
3120
3121
·'**************************************************************
'Function:Skip TABsandBLANKs.
Input:HL--Addresstobecheck.
Output:HL<-HL+?(?isthe numbersofTAB andBLANK).




3138-RETURNC-FLAGIF[A]ISNOTWITHIN{IAI••IzI}




;1
;A
;SPACE
;2
;S
;<--
;3
;D
;-->
;4
;F
;DOWNARROW
;5
;G
;UPARROW
;6
;H
;CR
;7
;J
;/
;8
;K
;<
;9
;L
;>
;0
,
;UNUSED
;Q
;Z
,
;W
;X
ii
;E
;C
;@
;R
;V

;T
;B








0B89	2E	3202K37	DEFB	2EH	,.
0B8A	7B	3203K38	DEFB	7BH	;UNUSED
0B88	50	3204K39	DEF8	50H	;P
0B8C	3F	3205  K3A	DEFB	3FH	;?
0B8D	7B	3206K78	DEFB	78H	;UNUSED
3207RTABLE:
088E	41	3208	DEFB	41H	;A
0B8F	46	3209	DEF8	46H	;F
0B90	42	3210	DEF8	42H	;B
0B91	43	3211	DEF8	43H	;C
0B92	44	3212	DEFB	44H	;D
0B93	45	3213	DEFB	45H	;E
0B94	48	3214	DEF8	48H	;H
0B95	4C	3215	DEFB	4CH	;L
0B96	60	3216	DEF8	60H	;A'
0B97	61	3217	DEFB	61H	;F'
0B98	62	3218	DEFB	62H	;B'
0899	63	3219	DEFB	63H	;C'
0B9A	64	3220	DEFB	64H	;D'
0B9B	65	3221	DEF8	65H	;E'
0B9C	66	3222	DEFB	66H	;H'
0B9D	67	3223	DEFB	67H	;L'
0B9E	49	3224	DEF8	49H	;I
089F	58	3225	DEF8	58H	;X
08A0	49	3226	DEFB	49H	;I
08Al	59	3227	DEFB	59H	;Y
0BA2	53	3228	DEFB	53H	;S
0BA3	50	3229	DEFB	50H	;P
0BA4	50	3230	DEFB	50H	;P
0BA5	43	3231	DEFB	43H	;C
0BA6	49	3232	DEFB	49H	;I
08A7	46	3233	DEFB	46H	;F
3234SEGTAB:
0BA8	FFFF	3235	DEFW	0FFFFH	;SPACE
08AA	FEFl	3236	DEFW	0FlFEH	;!
08AC	DFF7	3237	DEFW	0F7DFH	·,"
08AE	31FC	3238	DEFW	0FC31H	;#
0B80	12FC	3239	DEFW	0FC12H	;$
0BB2	1BC3	3240	DEFW	0C31BH·	;%
08B4	24E7	3241	DEFW	0E724H	;&
0BB6	FFF8	3242	DEFW	0FBFFH	•I
0BB8	FFEB	3243	DEFW	0E8FFH	;(
0BBA	FFD7	3244	DEFW	0D7FFH	;)
0BBC	3FC0	3245	DEFW	0C03FH	·I*
08BE	3FFC	3246	DEFW	0FC3FH	;+
08C0	FFDF	3247	DEFW	0DFFFH	;,
08C2	3FFF	3248	DEFW	0FF3FH	I
Ll)r:	OBJCODEMSTMTSOURCESTATEMENT	ASM5.9
0BC4	Fr'BF	3249	DEFW	0BFFFH	,.
0BC6	FFDB	3250	DEFW	0DBFFH	;/
0BC8	C0DB	3251	DEFW	0DBC0H	;0
0BCA	FFFC	3252	DEFW	0FCFFH	;1
0BCC	24FF	3253	DEFW	0FF24H	;2
0BCE	30FF	3254	DEFW	0FF30H	;3
08D0	19FF	3255	DEFW	0FF19H	;4
08D2	72F7	3256	DEFW	0F772H	;5
08D4	02FF	3257	DEFW	0FF02H	;6
0BD6	F8FF	3258	DEFW	0FFF8H	;7
0BD8	00FF	3259	DEFW	0FF00H	;8
0BDA	10FF	3260	DEFW	0FF10H	;9
0BDC	7FEF	3261	DEFW	0EF7FH	,
0BDE	BFDF	3262	DEFW	0DFBFH	;;
0BE0	F7DB	3263	DEFW	0DBF7H	;<
0BE2	37FF	3264	DEFW	0FF37H	;=
0BE4	F7E7	3265	DEFW	0E7F7H	;>
0BE6	7CFD	3266	DEFW	0FD7CH	;?
0BE8	A0FD	3267	DEFW	0FDA0H	;@
0BEA	08FF	3268	DEFW	0FF08H	;A
0BEC	70FC	3269	DEFW	0FC70H	;B
0BEE	C6FF	3270	DEFW	0FFC6H	;C
0BF0	F0FC	3271	DEFW	0FCF0H	;D
0BF2	06FF	3272	DEFW	0FF06H	;E
0BF4	0EFF	3273	DEFW	0FF0EH	;F
0BF6	42FF	3274	DEFW	0FF42H	;G
0BF8	09FF	3275	DEFW	0FF09H	;H
0BFA	F6FC	3276	DEFW	0FCF6H	;I
0BFC	ElF·F	3277	DEFW	0FFE1H	;J
0BFE	8FEB	3278	DEFW	0EBBFH	;K
0C00	C7FF	3279	DEFW	0FFC7H	;L
0C02	C9F3	3280	DEFW	0F3C9H	;M
0C04	C9E7	3281	DEFW	0E7C9H	;N
0C06	C0FF	3282	DEFW	0FFC0H	;O
0C08	0CFF	3283	DEFW	0FF0CH	;P
0C0A	C0EF	3284	DEFW	0EFC0H	;Q
0C0C	0CEF	3285	DEFW	0EF0CH	;R
0C0E	12FF	3286	DEFW	0FF12H	;S
0Cl0	FEFC	3287	DEFW	0FCFEH	;T
0Cl2	ClFF	3288	DEFW	0FFC1H	;U
0Cl4	CFDB	3289	DEFW	0DBCFH	;V
0Cl6	C9CF	3290	DEFW	0CFC9H	;W
0Cl8	FFC3	3291	DEFW	0C3FFH	;X
0ClA	FFFl	3292	DEFW	0FlFFH	;Y
0ClC	F6DB	3293	DEFW	0DBF6H	;Z
0ClE	FFCF	3294	DEFW	0CFFFH	,
0C20	FFE7	3295	DEFW	0E7FFH	;/
0C22	F0FF	3296	·oEFW	0FFF0H	;]
0C24	FFCD	3297	DEFW	0CDFFH	;
0C26	7FEB	3298	DEFW	0EB7FH	;<
0C28	08BF	3299	DEFW	0BF08H	;A'
0C2A	0Ei3F	3300	DEFW	·0BF0EH	;F'
0C2C	70BC	3301	DEFW	0BC70H	;B'
0C2E	C6BF	3302	DEFW	0BFC6H	;CI
0C30	F0BC	3303	DEFW	0BCF0H	;D'
0C32	06BF	33.04	DEFW	0BF06H	;E'
0C34	09BF	3305	DEFW	0BF09H	;H'
0C36	C7BF	3306	DEFW	0BFC7H	;L'
LOC	OBJCODEMSTMTSOURCESTATEMENT	ASM5.9

3307SHIFTT:
0C38	3C	3308	DEFB	3CH	;<
0C39	FF	3309	DEFB	0FFH
0C3A	3E	3310	DEFB	3EH	;>
0C3B	FF	3311	DEFB	0FFH
0C3C	2A	3312	DEFB	2AH	;*
0C3D	21	3313	DEFB	21H
,•.I
0C3E	22	3314	DEFB	22H	;ff
0C3F	23	3315	DEFB	23H	;lt
0C40	24	3316	DEFB	24H	;$
0C41	25	3317	DEFB	25H	;%
0C42	26	3318	DEFB	26H	;&
0C43	27	3319	DEFB	27H
,•I
0C44	28	3320	DEFB	28H	;(
0C45	29	3321	DEFB	29H	;)
0C46	3B	3322	DEFB	3BH	ii
0C47	FF	3323	DEFB	0FFH
0C48	FF	3324	DEFB	0FFH
0C49	FF	3325	DEFB	0FFH
0C4A	FF	3326	DEFB	0FFH
0C4B	2F	3327	DEFB	2FH	;/
0C4C	FF	3328	DEFB	0FFH
0C4D	FF	3329	DEFB	0FFH
0C4E	FF	3330	DEFB	0FFH
0C4F	FF	3331	DEFB	0FFH
0C50	FF	3332	DEFB	0FFH
0C51	FF	3333	DEFB	0FFH
0C52	FF	3334	DEFB	0FFH
0C53	FF	3335	DEFB	0FFH
0C54	FF	3336	DEFB	0FFH
0C55	2D	3337	DEFB	2DH	;-
0C56	FF	3338	DEFB	0FFH
0C57	5B	3339	DEFB	5BH	;
0C58	40	3340	DEFB	40H	;@
0C59	FF	3341	DEFB	0FFH
0C5A	FF	3342	DEFB	0FFH
0C5B
3D
3343
DEFB
3DH
;=
0C5C
2B
3344
DEFB
2BH
;+
3345MPFII:
0C5D	2A2A2A2A	3346	DEFM	'*****MPF'
0C65	2D	3347	DEFB	2DH	,
0C66	49	3348	DEFM	III
0C67	2D	3349	DEFB	2DH	;-
0C68	504C5553	3350	DEFM	'PLUS*****'
0C71	0D	3351	DEFB	0DH
3352ERRSP:
0C72	4552524F	3353	DEFM	'ERROR'
0C77	2D	3354	DEFB	2DH
0C78	5350	3355	DEFM	'SP'
0C7A	0D	3356	DEFB	0DH
3357SYSSP:
0C7B	535953	3358	DEFM	'SYS'
0C7E	2D	3359	DEFB	2DH
0C7F	5350	3360	DEFM	'SP'
0C81	0D	3361	DEFB	0DH
LOC	OBJCODEMSTMTSOURCESTATEMENT	ASM5.9


3365
PRTOFF:

0C89
50525420
3366
DEFM	'PRTOFF'

0C90
0D
3367
DEFB	0DH



3368
RAM2KVALUESET:

0C91
00F8
3369
DEFW	0F800H
;SET
EDITOR
LIMITS.
0C93
FFFC
3370
DEFW	0FCFFH



0C95
00FE
3371
DEFW	0FE00H
;SET
SYMBOL
LIMITS.
0C97
A0FE
3372
DEFW
0FEA0H
0C99
00FD
3373

DEFW
0FD00H
;SETOBJECT
LIMITS.
0C9B
FFFD
3374

DEFW
0FDFFH




3375
RAM4K
VALUE SET:



0C9D
00F0
3376

DEFW
0F000H
;SETEDITOR
LIMITS.
0C9F
FFFA
3377
DEFW
0FAFFH
0CA1
00FD
3378
DEFW
0FD00H
;SET
SYMBOL
LIMITS.
0CA3
A0FE
3379
DEFW
0FEA0H



0CA5
00FB
3380
DEFW
0FB00H
;SET
OBJECT
LIMITS.
0CA7	FFFC	3381	DEFW	0FCFFH
3382TENS	;TABLEUSEDBY'TOASCII'TOCONVERT

3383
;BINARY
TODECIMALDIGITS
0CA9
6400
3384
DEFW
100
0CAB
0A00
3385
DEFW
10
0CAD
0CAF
0100	3386	DEFW
20455252	3387ERRSMSGDEFM
1
IERRORS'
0CB6	0D	3388	DEFB	0DH






























3389
3390
3391
3392
3393
3394
3395
3396
3397
*HEADINGRAMSTORAGE
·I************************·I
.I*	*.I
;*	STORAGE	*;
;*	FOR MONITOR	*;
.*	*.
·************************·
FEA0

FEA0FED0
3398
3399
3400
3401
3402

USERSTK:

SYSSTK:
ORG

DEFSORG
0FEA0H
30H
0FED0H
FED0

FED9

FEDBFEDDFEDFFEE!FEE3FEESFEE?FEE9FEEB
3403
3404
3405
3406
3407
3408
3409
3410
3411
3412
3413
3414
3415
3416
STEPBFDEFSTEXTF
EDIT-START ADDRTEXT-T
END DATA ADDREND-LNNO
RAM-START ADDREDIT END ADDRST F
ST-T
OBJFOBJ-TEND-ADDR
9

DEFS

DEFSDEFSDEFSDEFSDEFSDEFSDEFSDEFSDEFS

;ASSEMBLERSOURCEFROM.
2	;EDITORBOTTOM.
;ASSEMBLERSOURCETO.
2	;EDITORTOP.
2	;EDITORLASTLINENUMBER.
2	;EDITORLOWLIMIT.
2	;EDITORHIGH LIMIT.
2	;ASSEMBLERSYMBOLTABLEFROM.
2	;ASSEMBLERSYMBOLTABLETO.
2	;ASSEMBLEROBJECTCODEFROM.
2	;ASSEMBLEROBJECTCODETO.
2	;Containsthelimitaddress
;ofccommandINSERTorDELETE•
FEEDFEEFFEF0FEFl

FEF2FEF3
3417
.3418
3419
3420
3421
3422
3423
BRADBRDAPOWERUPTEST

STEPFGPR.TFLG
DEFS	2
DEFS	1
DEFS	1
DEFS	1
DEFS	1
DEFS	1
;Breakpointaddress•
;Dataofbreakpointaddress•
;Powerupinitialization•
;Bit7---set whenillegalkey
;	isentered.
;STEPmodetestflag•
;Printertoggleswitch•
FEF4
3424
BEEPSETDEFS	1
;Beepsoundtoggleswitch.
FEF5FEF6FEF8FEFAFEFEFEFFFF01

FF03FF04FF2CFF7EFF80FF81FF82FF84FF86

FF88FF8AFF8C
3425
3426
3427
3428
3429
3430
3431
3432
3433
3434
3435
3436
3437
3438
3439
3440
3441
3442
3443
3444
3445
3446
FBEEPTBEEPMADORTEMPlATEMPHLTEMPIMlAD

RCOUNTINPBFDISPBFGETPTTYPEFGCRSETOUTPTRDISPINPTRREGBF:USERAFUSERBCUSERDE
DEFS	1
DEFS	2
DEFS	2
DEFS	4
DEFS	1
DEFS	2
DEFS	2
DEFS	1
DEFS	40
DEFS	82
DEFS	2
DEFS	1
DEFS	1
DEFS	2
DEFS	2
DEFS	2
DEFS	2
DEFS	2
DEFS	2
;FreqencyofBEEP•
;TimedurationofBEEP•
;Temporarystorage•
;SeecommentsoncommandSTEP.
;Temporarystorage•
;Temporarystorage.
;Containsthe addressofOpcode'FF'
;serviceroutine.(RST38H,mode
;1interrupt,etc).
;Registercountsinregistertable.
;Inputbuffer•
;Displaybuffer•
;Temporary storageforGETHL.
;Typetestflag•
;Displaydelaytime•
;Inputbufferpointer.
;Displaybufferpointer•
;Limitofinputbufferpointer•

FF8E
3447
USERHL
DEFS
2

FF90
3448
UAFP
DEFS
2
AF'
FF92
3449
UBCP
DEFS
2
BC'
FF94
3450
UDEP
DEFS
2
DE'
FF96
3451
UHLP
DEFS
2
HL'
FF98
3452
USERIX
DEFS
2

FF9A
3453
USERIY
DEFS
2

FF9C
3454
USERSP
DEFS
2

FF9E
3455
USERPC
DEFS
2

FFA0
3456
USERIF
DEFS
2


3457
BLANK
EQU
6FD0H

3458KTAB	EQU	068H	;TABCODE
3459TVSET	EQU	0A000H;Thefirstmemorylocation3460			;ofTV interfaceboard
3461TV	EQU	0A001H;Thestartingaddressof3462			;programonTVinterface
monitorboard
3463  BASICC	EQU	2020H
3464
;Thestartingaddressof
;reenterBASIC
                                ;                      ---- page 61 ----

; This file contains the API of the monitor program. Intended as a source 
; to be used in user programs. Not integral, only the lines required.

;MPF-IP-User-Manual 5.1 - System Parameters
; combined with data from Monitor Source Program Listing

USERSTK:EQU     0FEA0h ; 030h bytes
SYSSTK: EQU     0FED0h ; 
STEPBF: EQU     0FED4h ; Tape File Name
STPBFS: EQU     STEPBF+4 ; Tape Start Address
STPBFE: EQU     STEPBF+6 ; Tape Ending Address
STPBFC: EQU     STEPBF+8 ; Tape Checksum

TEXT_F:
EDSTAD: EQU     0FED9h ; Editor Bottom - Assembler Text Buffer From
TEXT_T:
EDEDDA: EQU     0FEDBh ; Editor Top - Assembler Text Buffer To
EDLNNO: EQU     0FEDDh ; Editor Last Line Number
EDRAMST:EQU     0FEDFh ; Editor Low Limit
EDENDAD:EQU     0FEE1h ; Editor High Limit

ST_F:
ASSTF:  EQU     0FEE3h ; Assembler Symbol Table From
ST_T:
ASSTT:  EQU     0FEE5h ; Assembler Symbol Table To
OBJ_F:
ASOBF:  EQU     0FEE7h ; Assembler Object Code From
OBJ_T:
ASOBT:  EQU     0FEE9h ; Assembler Object Code To
ENDADR: EQU     0FEEBh ; Limit of Insert and Delete

BRAD:   EQU     0FEEDh ; Break Point Address
BRDA:   EQU     0FEFFh ; Data Of Break Point Address

PWRUP:  EQU     0FEF0h ; Power Up Initialization
TEST:   EQU     0FEF1h ; Test Flag
STEPFG: EQU     0FEF2h ; Step Test Flag
PRTFLG: EQU     0FEF3h ; STEP mode test flag

BEEPST: EQU     0FEF4h ; Beep toggle switch
FBEEP:  EQU     0FEF5h ; Beep Frequency
TBEEP:  EQU     0FEF6h ; Time Duration of Beep

MADDR:  EQU     0FEF8h ; Temporary Storage
TEMP1:  EQU     0FEFAh ; Temporary Storage
ATEMP:  EQU     0FEFEh ; Temporary Storage
HLTEMP: EQU     0FEFFh ; Temporary Storage

IMIAD:  EQU     0FF01h ; Contains the address of opcode FF' Service Routine (RST38H)
RCOUNT: EQU     0FF03h ; Register Counter

INBUF:  EQU     0FF04h ; Input Buffer
DISPBF: EQU     0FF2Ch ; Display Buffer

GETPT:  EQU     0FF7Eh ; Check Hex pointer
TYPEFG: EQU     0FF80h ; Memory and Register Test Flags
CRSET:  EQU     0FF81h ; Display delay time
OUTPTR: EQU     0FF82h ; Input buffer pointer
DISP:   EQU     0FF84h ; Display buffer pointer
INPTR:  EQU     0FF86h ; Limit of input buffer pointer
REGBF:                  ; Register Buffer
USERAF: EQU     0FF88h ; 
USERBC: EQU     0FF8Ah ; 
USERDE: EQU     0FF8Ch ;
USERHL: EQU     0FF8Eh ; 
UAFP:   EQU     0FF90h ; AF'
UBCP:   EQU     0FF92h ; BC'
UDEP:   EQU     0FF94h ; DE'
UHLP:   EQU     0FF96h ; HL'
USERIX: EQU     0FF98h ; 
USERIY: EQU     0FF9Ah ; 
USERSP: EQU     0FF9Ch ; 
USERPC: EQU     0FF9Eh ; 
USERIF: EQU     0FFA0h ; 

EDITBF: EQU     0FFA2h ; RAM Buffer For Editor
ASSMBF: EQU     0FFB0h ; RAM Buffer For Assembler


;MPF-IP-User-Manual 5.2 - Subroutines

BEEP:   EQU     00803h ; Call TONE to generate sound.
CHK40:  EQU     00912h ; Check the number of contents in display buffer. If greater than 40, update IX pointer.
CHRWR:  EQU     00924h ; Convert ASCII value in (A) to pattern and store in DISPBF/INPBF. Call CURSOR.
CLEAR:  EQU     009B9H ; Clear DISPBF, reset DISP and OUTPTR.
CLRBF:  EQU     007F6h ; Call CLEAR, reset IX, call CHRWR.
CLRDSP: EQU     00840h ; Clear DISPBF
CONVER: EQU     00821h ; Convert ASCII value in (A) to pattern and store in DISPBF.

CR:     EQU     0093Bh ; Print out INPBF, check TV-interface, if present, jump to TV-INF-ISR.
CR1:    EQU     0097Ah ; As CR but with 1 sec display timing.
CR2:    EQU     00981h ; As CR but without call to CLEAR & CURSOR. 320 ms display timing.
CR3:    EQU     00985h ; As CR but call to CLRBG, not CLEAR. 480 ms display timing.
CURSOR: EQU     00A79h ; Get cursor message. Put it in DISP and DISP+1
DECBIN: EQU     00B28h ; Convert decimal ASCII in (DE) to hex at (HL), until a non-numeric value is found.
DECIMAL:EQU     00AB8h ; Convert hex values in (HL) to ASCII decimal in INPBF. Update OUTPTR.
DEC_SP: EQU     00399h ; Put FFh in (DISP) and (DISP)+1. This erases the cursor in the display buffer.
ERROR:  EQU     006C4h ; Print ERROR message and call PRTMES.
GETCHR: EQU     008AEh ; Find delimiter from (GETPTR). Valid are: SPACE, TAB, :, ., =, /. Put result in (HL).
GETHL:  EQU     008E5h ; Call GETCHR, Convert ASCII in (HL) to hex.
HEXBIN: EQU     00AF4h ; Convert ASCII codes from (DE) to hex values until a non-hex digit is found. Result in HL.
HEX1:   EQU     00AADh ; Convert lower 4 bits from A to ASCII and display using CHRWR.
HEX2:   EQU     00A9Ah ; Convert A to ASCII and display using CHRWR. Uses HEX1.
HEX4:   EQU     00A92h ; As HEXX but adds space.
HEXX:   EQU     00A89h ; Convert HL to ASCII and display using CHRWR. Uses HEX2.
LDA:    EQU     008B1h ; As GETCHR but sets HL directly.
MSG:    EQU     009CAH ; Convert ASCII from INPBF to display patterns in DISPBF, until a <CR> is found. HL is pointer.

ONE:    EQU     00B14h ; Converts ASCII in A to hex digit. Carry flag is set for non-hes values.

PRINTT: EQU     00893h ; Call PTEST. If a PRT-MPF is connected, print display buffer.
PRTMES: EQU     00886h ; Call MSG. If a PRT-MPF is connected, output to printer too.
PTEST:  EQU     008A3h ; Check the printer toggle switch. If on, call PTESTT.
PTESTT: EQU     008A8h ; Check if a PRT-MPF is connected. If yes, set Z-flag to 1.

RAMCHK: EQU     00819h ; Test memory locationin (HL) to be RAM. If yes, set Z-flag to 1.
READLN: EQU     009D4h ; Copy a string delimited by <CR> from OUTPTR to INPTR.

SCAN:   EQU     00246H ; Call SCAN2 and BEEP.
SCAN1:  EQU     0029BH ; Scan keyboard once. Set carry flag to 1 if a key is pressed. Returned key as a position code.
SCAN2:  EQU     0024Dh ; Display DISPBF and scan keybord until a key is pressed. Returns an ASCII code.

SKIP:   EQU     00B40h ; Use (HL) to skip whitespace (TAB, SPACE). Returns first non whitespace in HL.
SPACE1: EQU     00A95h ; Load SPACE into A and calls CHRWR.

TONE:   EQU     00874h ; Generate a square wave to MIC output and speaker. C specifies the frequency.
TONE1K: EQU     0086Fh ; Generate 1 kHz. Duration is in HL.
TONE2K: EQU     00872h ; Generate 2 kHz. Duration is in HL.

; PRT-MPF-IP only
MTPPRT: EQU     06A40h ; Print memory contents until a <CR> is found.
PLINE:  EQU     06A30h ; Calls PLINEFD twice.
PLINEFD:EQU     06A10h ; Perform a line feed.
SHIFT:  EQU     06A0Dh ; Shift PRT-MPF print head to the right. Amount is in B.

; TVA/TVB-MPF-IP only
TVSET:  EQU     0A000H  ; The first memory location
                        ; of TV interface board .
TV:     EQU     0A001H  ; The starting address of monitor
                                ; program on TV interface board .
                                
; BASIC
BASICC: EQU     02020H  ;The starting address of
                        ;reenter BASIC 

ESCAPE: EQU     000E6h  ; Re-enter monitor

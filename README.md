!MicroProfessor MPF-I-Plus monitor listing

This is the listing in ASCII format, and will assemble with z80asm
(https://github.com/udo-munk/z80pack).

The listing covers the first 4 kByte which is the monitor, excluding the 
assembler and disassembler. These are in the second 4 kByte. Together they
are placed in the 8 kByte ROM in socket U2. Basic is in Socket U3.

The original scans and pdf and docx documents can be found at:

https://pool378.seedbox.fr/files/index.php/s/Rx3poMMH34XbpDq?path=%2F

The docx document is used to get the initial text, the pdf document to
correct it. The OCR isn't very good for code listings, alas.

The OCR in the .docx file is quite poor, but with the .jpg's available,
tesseract can do better:

    tesseract   'MPF-IP MONITOR SOURCE PROGRAM LISTING_38.jpg' page35 --psm 6

This forces OCR in single column mode. 

Changes made to the code for z80asm:

* added a colon to all labels
* replaced the '?' in some labels with 'Q' or '_Q'

Phases in creating a correct binary image:

* Editing OCR to match original,
* Generating an assembly without errors,
* Find typos by matching generated hex with ROM derived hex.

The latter is done in an iterative loop:

* generate an uniform hex file from source:
  z80asm -fb -omonitorIP.bin -l monitorIP.asm
  perl bin2intelHex2.pl monitorIP.bin > monitorIP.hex
* compare with ROM hexfile:
  meld monitorIP.hex2 MPF1P_rom_moniteur_U2.hex2
* find and fix the difference in generated list file and scan
* loop until no difference. Meanwhile, try to keep line numbers matching


Differences between scanned listing and ROM:

    1032 MDUMP1:
        CALL    PTESTT ; PTEST in listing
        
    1730 MEMDP3: JP      RFOR3  ; REGALL in listing
   
    2548 CHK46:
        LD      DE,DISPBF+38    ;LD      HL, (DISP)    ; order in listing
        LD      IX,DISPBF       ;LD      DE, DISPBF+38
        LD      HL, (DISP)      ;AND     A
        AND     A               ;SBC     HL,DE           ;  ---- page 44 ----
        SBC     HL,DE           ;LD      IX, DISPBF
        
    3442 INPBF:  DEFS    38              ;Input buffer .   listing = 40

As yet unresolved: According the listing DISPBF starts at 0FF2Ch, but the 
ROM disassembly has 0FF2Ah. For now the listing value is maintained.

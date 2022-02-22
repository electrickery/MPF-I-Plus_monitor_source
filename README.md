The original scans and pdf and docx documents can be found at:

https://pool378.seedbox.fr/files/index.php/s/Rx3poMMH34XbpDq?path=%2F

The docx document is used to get the initial text, the pdf document to
correct it. The OCR isn't very good for code listings, alas.

The OCR in the .docx file is quite poor, but with the .jpg's available,
tesseract can do better:

    tesseract   'MPF-IP MONITOR SOURCE PROGRAM LISTING_38.jpg' page35 --psm 4

This forces one column mode. '--psm 6' works even better.

Changes made to the code for z80asm:

* added a colon to all labels
* replaced the '?' in some labels with a '_Q'


'WARNING. THIS PATCH WAS DONE FOR V.ALPHA 4 ONLY.
'         DON'T USE IT IN ANY OTHER VERSION OR YOU CAN DAMAGE THE PROGRAM
'In this record is saved the default font name: 135295 to 135306
'How do I know!? Well... I code it!
a$ = "quickpt4.exe"
ON ERROR GOTO wrong
DIM k AS STRING * 1
CLS
OPEN a$ FOR BINARY ACCESS READ AS #1
FOR i& = 135295 TO 135306
 GET #1, i&, k
 DefFontName$ = DefFontName$ + k
NEXT i&
CLOSE #1
COLOR 7, 1
LOCATE 1, 25: PRINT "[<QUICKPT4 PATCH BY JONATHAN GALLEGOS>]"
PRINT "This patch rewrite the EXE file to change the default font"
PRINT "because in this version the font can't be changed yet"
PRINT "Default QUICKPAINT Font: "; DefFontName$
COLOR 7, 0
PRINT
 FILES
PRINT
COLOR 7, 1
PRINT "Choose an available font otherwise QUICK PAINT could crash! :("
INPUT ">New Default Font: ", NewFontName$
IF NewFontName$ = "" THEN
 PRINT "No font info provided"
 END
END IF
OPEN NewFontName$ FOR INPUT AS #2
CLOSE #2
 
' FOR l% = 1 TO 12
'  IF MID$(NewFontName$, l%, 1) = CHR$(32) THEN EXIT FOR
'  PRINT MID$(NewFontName$, l%, 1);
' NEXT l%
'TheNewFontInfo$ = SPACE$(12)
 FOR l% = 1 TO 12
   IF MID$(NewFontName$, l%, 1) = CHR$(32) THEN EXIT FOR
   TheNewFontInfo$ = TheNewFontInfo$ + MID$(NewFontName$, l%, 1)
 NEXT l%

 IF LEN(TheNewFontInfo$) < 12 THEN
   DO
    TheNewFontInfo$ = TheNewFontInfo$ + CHR$(32)
    IF LEN(TheNewFontInfo$) = 12 THEN EXIT DO
   LOOP
 END IF

OPEN a$ FOR BINARY ACCESS WRITE AS #1
  PUT #1, 135295, TheNewFontInfo$
CLOSE #1


PRINT "Default font is: "; NewFontName$
PRINT "Font info succesfully changed!"
END

wrong:
COLOR 7, 1
SELECT CASE ERR
CASE 53
  PRINT "Can't find the file!"
  PRINT "Be sure that PATCH.EXE, QUICKPT4.EXE and *.FNT files are in the same directory"
CASE ELSE
 PRINT "Error:"; ERR; "on line"; ERL
END SELECT
END


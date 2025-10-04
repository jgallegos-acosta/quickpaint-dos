'** If you are reading this message you loaded the wrong module
'** It doesn't do anything by itself.
'** PLEASE, In order to run the full application you need to
'** to load QUICKPT4.BAS....

DECLARE SUB HalfToneButton (x1%, y1%, w%, h%, F%, B%, L%, s%, S2%)
DECLARE SUB RoundBox (x%, y%, w%, h%, F%, L%, s%, S2%)
DECLARE SUB SetMouse (Action%, var1%, var2%)
DECLARE SUB GetUndoMem ()
DECLARE SUB PutUndoMem ()
DECLARE FUNCTION GetMaxStrVal% (txt$, FontFile$, Attribs%)
DECLARE SUB WinFont (txt$, x%, y%, Clr%, bold%)
DECLARE SUB FButon (x1%, y1%, w%, h%, bck%, light%, shadow%, shadow2%)
DECLARE SUB Fill (xp%, yp%, fc%, bc%)
DEFINT A-Z
DECLARE SUB DrawEllipse (oX%, oY%, prx%, pry%, co%)
DECLARE SUB Millidelay (msecs%)
DECLARE SUB Save640 (File$)
DECLARE SUB Load640 (File$)
DECLARE SUB MoveWin (ID%)
DECLARE SUB QFont (txt$, x%, y%, c%, FontFile$, Attribs%)
DECLARE SUB Mouse (Action%)
DECLARE SUB MouseStatus (mb%, x%, y%)
DECLARE FUNCTION MSOverArea% (x1%, y1%, x2%, y2%)
DECLARE FUNCTION ChangeBit$ (BYTE AS STRING, WhichBit AS INTEGER, ChangeTo AS INTEGER)
DECLARE FUNCTION CheckBit% (BYTE AS STRING, WhichBit AS INTEGER)
'****************************************************************************
CONST Show = 1, Hide = 2 'Constants for Mouse SUB
CONST Position = 4, HorzLimit = 7, VertLimit = 8 'Constants for SetMouse SUB
COMMON SHARED PAX1%, PAY1%, PAX2%, PAY2% 'Paint Area Coords
COMMON SHARED BackClr%, ForeClr%, TheImgName$, FileModified%
COMMON SHARED TheFontPath$, StdFnt$, WinFnt$, SwapVideoFile$
COMMON SHARED UndoCount%, AcTool%
TYPE pts
  x AS INTEGER
  y AS INTEGER
  c AS INTEGER
END TYPE
DIM SHARED sam(105) AS pts
'****************************************************************************
'$DYNAMIC
ON ERROR GOTO Wrong

END

Wrong:
 PRINT ERR
 END

REM $STATIC
SUB DrawEllipse (oX, oY, prx, pry, co)
  DIM xe AS LONG, ye AS LONG, E AS LONG
  IF pry = 0 THEN 'special cases for horizontal & vertical ellipses
    LINE (oX - prx, oY)-(oX + prx, oY), co
    EXIT SUB
  END IF
  IF prx = 0 THEN
    LINE (oX, oY - pry)-(oX, oY + pry), co
    EXIT SUB
  END IF
  'work with largest axis to avoid rounding errors
  IF pry <= prx THEN
    x = 0: y = pry
    xe = 0: ye = CLNG(prx) * prx
    E = -ye \ 2: c# = ye \ pry
    DO
      IF E <= 0 THEN
        DO
          PSET (oX + x, oY + y), co: PSET (oX - x, oY + y), co
          PSET (oX + x, oY - y), co: PSET (oX - x, oY - y), co
          x = x + 1
          xe = xe + pry
          E = E + xe
        LOOP WHILE E <= 0
      ELSE
        PSET (oX + x, oY + y), co: PSET (oX - x, oY + y), co
        PSET (oX + x, oY - y), co: PSET (oX - x, oY - y), co
      END IF
      y = y - 1
      ye = ye - c#
      E = E - ye
    LOOP UNTIL y = 0
    PSET (oX + x, oY), co: PSET (oX - x, oY), co
    PSET (oX + x, oY), co: PSET (oX - x, oY), co
  ELSE
    x = 0: y = prx
    xe = 0: ye = CLNG(pry) * pry
    E = -ye \ 2: c# = ye \ prx
    DO
      IF E <= 0 THEN
        DO
          PSET (oX + y, oY + x), co: PSET (oX - y, oY + x), co
          PSET (oX + y, oY - x), co: PSET (oX - y, oY - x), co
          x = x + 1
          xe = xe + prx
          E = E + xe
        LOOP WHILE E <= 0
      ELSE
        PSET (oX + y, oY + x), co: PSET (oX - y, oY + x), co
        PSET (oX + y, oY - x), co: PSET (oX - y, oY - x), co
      END IF
      y = y - 1
      ye = ye - c#
      E = E - ye
    LOOP UNTIL y = 0
    PSET (oX, oY + x), co: PSET (oX, oY + x), co
    PSET (oX, oY - x), co: PSET (oX, oY - x), co
  END IF
END SUB

SUB RoundBox (x%, y%, w%, h%, F%, L%, s%, S2%)
'LINE (x% + 2, y% + 1)-(x% + W% - 2, y% + h% - 1), F%, BF
LINE (x% + 2, y% + 1)-(x% + w% - 2, y% + h% - 1), s%
'Gradient x% + 2, y% + 2, x% + W% - 2, y% + h% - 2, Clr1%, Clr2%, 100, 2
LINE (x% + 2, y%)-(x% + w% - 2, y%), s%
LINE (x%, y% + 2)-(x%, y% + h% - 2), s%
LINE (x% + w%, y% + 2)-(x% + w%, y% + h% - 2), s%
LINE (x% + 2, y% + h%)-(x% + w% - 2, y% + h%), s%
 PSET (x% + 1, y% + 1), s%
 PSET (x% + 1, y% + h% - 1), s%
 PSET (x% + w% - 1, y% + 1), s%
 PSET (x% + w% - 1, y% + h% - 1), s%
'LINE (x% + 3, y% + 1)-(x% + W% - 3, y% + 1), L%
'LINE (x% + 1, y% + 3)-(x% + 1, y% + h% - 3), L%
'LINE (x% + W% - 1, y% + 3)-(x% + W% - 1, y% + h% - 3), S2%
'LINE (x% + 3, y% + h% - 1)-(x% + W% - 3, y% + h% - 1), S2%
' PSET (x% + 2, y% + 2), L%
' PSET (x% + W% - 2, y% + 2), L%
' PSET (x% + 2, y% + h% - 2), S2%
' PSET (x% + W% - 2, y% + h% - 2), S2%
END SUB

SUB SaveScr (Filename$, sx, sy, ex, ey, nbits, imgnum)
IF Filename$ = "" THEN
   Filename$ = "CAP" + LTRIM$(RTRIM$(STR$(imgnum))) + ".BMP"
   imgnum = imgnum + 1
END IF
OPEN Filename$ FOR BINARY AS #1
IF LOF(1) <> 0 THEN
   'Alter this code here if you don't want it to overwrite existing files.
   CLOSE 1
   KILL Filename$
   OPEN Filename$ FOR BINARY AS #1
END IF

va = &H3C7 'VGA Palette Read Address Register
vd = &H3C9 'VGA Palette Data Register

zero$ = CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0)

'Check extents to order points.
IF sx > ex THEN SWAP sx, ex
IF sy > ey THEN SWAP sy, ey

'Use Windows BMP Header. Size=40
headersize = 40

'Calculate Picture width,height
picwidth = ex - sx + 1
picheight = ey - sy + 1

'Set Colour Information
'Planes [W] - Must be 1
nplanes = 1

'Calculate offset [LW] to start of data
IF nbits = 1 OR nbits = 4 OR nbits = 8 THEN
   offset = 14 + headersize + 4 * (2 ^ nbits)
ELSE
   offset = 14 + headersize
END IF

'Type of file [W] (Should be BM)
ft$ = "BM"

'File Size [LW] (excluding header)
IF nbits = 1 THEN
   IF (picwidth MOD 32) <> 0 THEN
      FileSize = 4 * (INT(picwidth / 32) + 1) * picheight
   ELSE
      FileSize = (picwidth / 8) * picheight
   END IF
ELSEIF nbits = 4 THEN
   IF (picwidth MOD 8) <> 0 THEN
      FileSize = 4 * (INT(picwidth / 8) + 1) * picheight
   ELSE
      FileSize = (picwidth / 2) * picheight
   END IF
ELSEIF nbits = 8 THEN
   IF (picwidth MOD 4) <> 0 THEN
       FileSize& = 4 * (INT(picwidth / 4) + 1) * picheight
   ELSE
      FileSize& = picwidth * picheight
   END IF
ELSEIF nbits = 24 THEN
   IF (3 * picwidth MOD 4) <> 0 THEN
      FileSize = 4 * (INT(3 * picwidth / 4) + 1) * picheight
   ELSE
      FileSize = 3 * picwidth * picheight
   END IF
END IF

'Set reserved values [W] (both must be zero)
r1 = 0
r2 = 0

'Compression type [LW] - None
comptype = 0

'Image Size [LW]; Scaling Factors xsize, ysize unused.
imagesize& = offset + FileSize&
xsize = 0
ysize = 0

'Assume all colours used [LW] - 0 means all colours.
coloursused = 0
neededcolours = 0

header$ = ft$ + MKL$(FileSize&) + MKI$(r1) + MKI$(r2) + MKL$(offset)
infoheader$ = MKL$(headersize) + MKL$(picwidth) + MKL$(picheight) + MKI$(nplanes)
infoheader$ = infoheader$ + MKI$(nbits) + MKL$(comptype) + MKL$(imagesize&)
infoheader$ = infoheader$ + MKL$(xsize) + MKL$(ysize) + MKL$(coloursused)
infoheader$ = infoheader$ + MKL$(neededcolours)

'Write headers to BMP File.
PUT #1, 1, header$
PUT #1, , infoheader$

'Add palette - Get colours (Write as B0G0R0(0),B1G1R1(0),...)
IF nbits = 1 OR nbits = 4 OR nbits = 8 THEN
   palet$ = ""
   OUT va, 0
   FOR count = 1 TO 2 ^ nbits
      zr = INP(vd) * 4
      zg = INP(vd) * 4
      zb = INP(vd) * 4
      palet$ = palet$ + CHR$(zb) + CHR$(zg) + CHR$(zr) + CHR$(0)
   NEXT count
   PUT #1, , palet$
   palet$ = "" 'Save some memory
END IF


stpoint = POINT(sx, ey + 1)

'BMPs are arranged with the top of the image at the bottom of the file.
'Get points off the screen and pack into bytes depending on the number of
'bits used. Deal with unused bits at the end of the line.
'Check for invalid range.
FOR count2 = ey TO sy STEP -1
   lin$ = ""
   IF nbits = 1 THEN
      count1 = sx
      WHILE count1 <= ex
         IF count1 + 7 > ex THEN
            T = 0
            FOR count0 = 0 TO 7
               p = POINT(count1 + count0, count2)
               IF p < 0 THEN p = 0
               T = T + (2 ^ (7 - count0)) * (p MOD 2)
            NEXT count0
            t2 = ex - count1 + 1
            T = T AND ((2 ^ t2) - 1) * (2 ^ (8 - t2))
            lin$ = lin$ + CHR$(T)
         ELSE
            T = 0
            FOR count0 = 0 TO 7
               p = POINT(count1 + count0, count2)
               IF p < 0 THEN p = 0
               T = T + (2 ^ (7 - count0)) * (p MOD 2)
            NEXT count0
            lin$ = lin$ + CHR$(T)
         END IF
         count1 = count1 + 8
      WEND
   ELSEIF nbits = 4 THEN
      count1 = sx
      WHILE count1 <= ex
         IF count1 = ex THEN
            p = POINT(count1, count2)
            IF p < 0 THEN p = 0
            lin$ = lin$ + CHR$((p MOD 16) * 16)
         ELSE
            p = POINT(count1, count2)
            p2 = POINT(count1 + 1, count2)
            IF p < 0 THEN p = 0
            IF p2 < 0 THEN p2 = 0
            lin$ = lin$ + CHR$((p MOD 16) * 16 + p2)
         END IF
         count1 = count1 + 2
      WEND
   ELSEIF nbits = 8 THEN
      FOR count1 = sx TO ex
         p = POINT(count1, count2)
         IF p < 0 THEN p = 0
         lin$ = lin$ + CHR$(p)
      NEXT count1
   ELSEIF nbits = 24 THEN
      'I'm not sure what to put here. QBasic doesn't support truecolour
      'Unused for now.
   END IF

'****************************************************************************
   Pcnt& = ABS(count2)
   Pcnt2& = 100
   Percent& = 110 - ((Pcnt& * Pcnt2&) / ey)
   FButon 78, 456, 32, 15, 7, 7, 7, 7
   WinFont STR$(Percent&) + "", 76, 458, 0, 1
'****************************************************************************


   'Pad line to LongWord boundary
   IF (LEN(lin$) MOD 4) <> 0 THEN
      lin$ = lin$ + MID$(zero$, 1, 4 - (LEN(lin$) MOD 4))
   END IF

    'Indicate our status
    'PSET (sx, count2 + 1), stpoint
    'stpoint = POINT(sx, count2)
   'IF nbits = 8 THEN
      'PSET (sx, count2), 255 - stpoint
   'ELSEIF nbits = 4 THEN
      'PSET (sx, count2), 15 - stpoint
   'ELSEIF nbits = 1 THEN
      'PSET (sx, count2), 1 - stpoint
   'END IF

   'Write the current line to the BMP file
   PUT #1, , lin$
NEXT count2

'Save some memory
lin$ = ""

PSET (sx, count2 + 1), stpoint

'Close the file
CLOSE
END SUB

SUB Tool.AirBrush
  RANDOMIZE TIMER
  PI = 3.1415926539#
  Cons! = PI / 180
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
     MouseStatus mb%, mx%, my%
     IF mb% = 1 THEN DrawClr% = ForeClr%
     IF mb% = 2 THEN DrawClr% = BackClr%
     Radius% = 10
     DO
       MouseStatus mb%, mx%, my%
        i% = INT(RND * 370)
        R% = INT(RND * Radius%)
        Millidelay 2
        Mouse Hide
        PSET (mx% + R% * COS(Cons! * i%), my% - R% * SIN(Cons! * i%)), DrawClr%
        Mouse Show
     LOOP UNTIL mb% = 0
  VIEW
END SUB

SUB Tool.Brush
BrushWidth% = 10
BrushType% = 1
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
    MouseStatus mb%, mx%, my%
     IF mb% = 1 THEN DrawClr% = ForeClr%
     IF mb% = 2 THEN DrawClr% = BackClr%
    oX% = mx%: oY% = my%
    DO
      MouseStatus mb%, mx%, my%
      IF (mx% <> nx%) OR (my% <> ny%) THEN
      SELECT CASE BrushType%
      CASE 1
        Mouse Hide
         FOR u% = 1 TO BrushWidth%
           FOR v% = 1 TO BrushWidth%
              LINE (oX% + u%, oY% + v%)-(mx% + u%, my% + v%), DrawClr%
           NEXT v%
         NEXT u%
        Mouse Show
      CASE 2
        Mouse Hide
          FOR R% = 1 TO 10
            LINE (oX% + R%, oY%)-(mx% + R%, my%), DrawClr%
          NEXT R%
        Mouse Show
      CASE 3
        Mouse Hide
          FOR R% = 1 TO 10
            LINE (oX%, oY%)-(mx% + R%, my% + R%), DrawClr%
          NEXT R%
        Mouse Show
      END SELECT
      END IF
      nx% = mx%: ny% = my%
      oX% = mx%: oY% = my%
    LOOP UNTIL mb% = 0
  VIEW
END SUB

SUB Tool.Curve
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
    LineMemFile$ = "\memscr"
    Mouse Hide
      Save640 LineMemFile$
    Mouse Show
    MouseStatus mb%, mx%, my%
    IF mb% = 1 THEN DrawClr% = ForeClr%
    IF mb% = 2 THEN DrawClr% = BackClr%
    DO
       MouseStatus mb%, mx%, my%
    LOOP UNTIL mb% <> 0
    MouseStatus mb%, x00, y00
    Mouse Hide
      PSET (x00, y00), DrawClr%
    Mouse Show
    DO
      MouseStatus mb%, mx%, my%
    LOOP UNTIL mb% = 0
    DO
      MouseStatus mb%, mx%, my%
    LOOP UNTIL mb% <> 0
    MouseStatus mb%, x03, y03
    Mouse Hide
      PSET (x03, y03), DrawClr%
    Mouse Show
    DO
       MouseStatus mb%, x01, y01
       MouseStatus mb%, mx%, my%
       IF x01 <> ox01 OR y01 <> ox01 THEN
       x02 = x01
       y02 = y01        ' make these equal for a standard curve
       a = x03 - 3 * x02 + 3 * x01 - x00
       E = y03 - 3 * y02 + 3 * y01 - y00
       B = 3 * x02 - 6 * x01 + 3 * x00
       F = 3 * y02 - 6 * y01 + 3 * y00
       c = 3 * x01 - 3 * x00
       g = 3 * y01 - 3 * y00
       D = x00
       h = y00
       cnt = 0
       FOR T! = 0 TO 1 STEP .01
         xb = ((a * T! + B) * T! + c) * T! + D
         yb = ((E * T! + F) * T! + g) * T! + h
         IF POINT(xb, yb) <> 255 THEN
           cnt = cnt + 1
           sam(cnt).x = xb
           sam(cnt).y = yb
           sam(cnt).c = POINT(xb, yb)
           'PSET (sam(cnt).x, sam(cnt).y), DrawClr%
           LINE -(sam(cnt).x, sam(cnt).y), DrawClr%
         END IF
       NEXT T!
       IF mx% <> nx% OR my% <> ny% THEN
        Mouse Hide
         Load640 LineMemFile$
        Mouse Show
        END IF
        PSET (x00, y00), DrawClr%
       END IF
       Mouse Show
       nx% = mx%: ny% = my%
       ox01 = x01: oy01 = y01
     LOOP UNTIL mb% = 0
     Mouse Hide
     FOR L = 1 TO cnt
       LINE -(sam(L).x, sam(L).y), DrawClr%
     NEXT
     LINE -(x03, y03), DrawClr%
     Mouse Show
     KILL LineMemFile$ + ".blu"
     KILL LineMemFile$ + ".grn"
     KILL LineMemFile$ + ".red"
     KILL LineMemFile$ + ".int"
  VIEW
END SUB

SUB Tool.Dropper
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
     DO
        MouseStatus mb%, mx%, my%
        IF mx% <> nx% OR my% <> ny% THEN
          Mouse Hide
             DClr% = POINT(mx%, my%)
             VIEW
             IF DClr% <> -1 THEN FButon 5 + 1, 240 + 1, 50 - 3, 80 - 3, DClr%, DClr%, DClr%, DClr%
             VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
             IF mb% = 1 THEN ForeClr% = DClr%
             IF mb% = 2 THEN BackClr% = DClr%
          Mouse Show
        END IF
        nx% = mx%: ny% = my%
     LOOP UNTIL mb% = 0
  VIEW
     Mouse Hide
     FButon 23, 423, 15, 15, BackClr%, 15, 0, BackClr%
     FButon 13, 413, 15, 15, ForeClr%, 15, 0, ForeClr%
     Mouse Show
'=========== The Tool's Settings
  FButon 5 + 1, 240 + 1, 50 - 3, 80 - 3, 7, 7, 7, 7
END SUB

SUB Tool.Ellipse
LineWidth% = 0
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
     LineMemFile$ = "\memscr"
     MouseStatus mb%, mx%, my%
     IF mb% = 1 THEN DrawClr% = ForeClr%
     IF mb% = 2 THEN DrawClr% = BackClr%
     oX% = mx%: oY% = my%
     Mouse Hide
     Save640 LineMemFile$
     Mouse Show
     DO
       MouseStatus mb%, mx%, my%
       IF mx% <> nx% OR my% <> ny% THEN
         prx% = ABS(mx% - oX%)
         pry% = ABS(my% - oY%)
          Mouse Hide
          Load640 LineMemFile$
          LOCATE 27, 60: COLOR 7: PRINT "�";
          'FOR i% = 0 TO LineWidth%
            DrawEllipse oX%, oY%, prx% + i%, pry% + i%, DrawClr%
          'NEXT i%
          Mouse Show
       END IF
       nx% = mx%: ny% = my%
     LOOP UNTIL mb% = 0
     KILL LineMemFile$ + ".blu"
     KILL LineMemFile$ + ".grn"
     KILL LineMemFile$ + ".red"
     KILL LineMemFile$ + ".int"
  VIEW
END SUB

SUB Tool.Eraser
EraserWidth% = 4
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
    MouseStatus mb%, mx%, my%
    DrawClr% = BackClr%
    oX% = mx%: oY% = my%
    DO
      MouseStatus mb%, mx%, my%
      IF (mx% <> nx%) OR (my% <> ny%) THEN
        Mouse Hide
         FOR u% = 1 TO EraserWidth%
           FOR v% = 1 TO EraserWidth%
              LINE (oX% + u%, oY% + v%)-(mx% + u%, my% + v%), DrawClr%
           NEXT v%
         NEXT u%
        Mouse Show
      END IF
      nx% = mx%: ny% = my%
      oX% = mx%: oY% = my%
    LOOP UNTIL mb% = 0
  VIEW
END SUB

'SUB Tool.Fill
'  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
'     MouseStatus mb%, mx%, my%
'     IF mb% = 1 THEN DrawClr% = ForeClr%
'     IF mb% = 2 THEN DrawClr% = BackClr%
'     LINE (PAX1%, PAY1%)-(PAX2% - 1, PAY2% - 1), DrawClr%, B
'     Mouse Hide
'     PAINT (mx%, my%), DrawClr%, BackClr%
'     Mouse Show
'     LINE (PAX1%, PAY1%)-(PAX2% - 1, PAY2% - 1), 0, B
'     DO
'       MouseStatus mb%, mx%, my%
'     LOOP UNTIL mb% = 0
'  VIEW
'END SUB
SUB Tool.Fill
VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
  MouseStatus mb%, mx%, my%
  IF mb% = 1 THEN DrawClr% = ForeClr%
  IF mb% = 2 THEN DrawClr% = BackClr%

  IF mx% < PAX1% + 1 OR mx% > PAX2% - 2 OR my% < PAY1% + 1 OR my% > PAY2% - 2 THEN
      VIEW: EXIT SUB
  END IF

  old% = POINT(mx%, my%)
  IF old% = DrawClr% THEN VIEW: EXIT SUB

  Mouse Hide

  'No border
  PAINT (mx%, my%), DrawClr%

  ' temporal border
  ' LINE (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2), 15, B
  ' PAINT (mx%, my%), DrawClr%, 15
  ' LINE (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2), 0, B

  Mouse Show
  DO: MouseStatus mb%, mx%, my%: LOOP UNTIL mb% = 0

  VIEW
END SUB

SUB Tool.Line
LineWidth% = 2
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
     LineMemFile$ = "\memscr"
     MouseStatus mb%, mx%, my%
     IF mb% = 1 THEN DrawClr% = ForeClr%
     IF mb% = 2 THEN DrawClr% = BackClr%
     oX% = mx%: oY% = my%
     Mouse Hide
     Save640 LineMemFile$
     Mouse Show
     DO
       MouseStatus mb%, mx%, my%
       IF mx% <> nx% OR my% <> ny% THEN
          Mouse Hide
          Load640 LineMemFile$
          LINE (oX%, oY%)-(mx%, my%), DrawClr%
          'FOR u% = 1 TO LineWidth%
          '   FOR v% = 1 TO LineWidth%
                LINE (oX% + u%, oY% + v%)-(mx% + u%, my% + v%), DrawClr%
          '   NEXT v%
          'NEXT u%
          Mouse Show
       END IF
       nx% = mx%: ny% = my%
     LOOP UNTIL mb% = 0
     KILL LineMemFile$ + ".blu"
     KILL LineMemFile$ + ".grn"
     KILL LineMemFile$ + ".red"
     KILL LineMemFile$ + ".int"
  VIEW
END SUB

SUB Tool.Pencil
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
     MouseStatus mb%, mx%, my%
     IF mb% = 1 THEN DrawClr% = ForeClr%
     IF mb% = 2 THEN DrawClr% = BackClr%
     PSET (mx%, my%), DrawClr%
    DO
      MouseStatus mb%, mx%, my%
      IF (mx% <> nx%) OR (my% <> ny%) THEN
        Mouse Hide
        LINE -(mx%, my%), DrawClr%
        Mouse Show
        PSET (mx%, my%), DrawClr%
      END IF
      nx% = mx%: ny% = my%
    LOOP UNTIL mb% = 0
  VIEW
END SUB

SUB Tool.Polygon
STATIC NewPoly, oldX%, oldY%, FirstX%, FirstY%
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
     LineMemFile$ = "\memscr"
     MouseStatus mb%, mx%, my%
     IF mb% = 1 THEN DrawClr% = ForeClr%
     IF mb% = 2 THEN DrawClr% = BackClr%
     IF NewPoly = 0 THEN
       FirstX% = mx%: FirstY% = my%
       oldX% = mx%: oldY% = my%
       NewPoly = 1
     END IF
     Mouse Hide
       Save640 LineMemFile$
     Mouse Show
     DO
       MouseStatus mb%, mx%, my%
       IF mx% <> nx% OR my% <> ny% THEN
          Mouse Hide
          Load640 LineMemFile$
          LINE (oldX%, oldY%)-(mx%, my%), DrawClr%
          LINE (oldX%, oldY%)-(mx%, my%), DrawClr%
          Mouse Show
       END IF
       nx% = mx%: ny% = my%
     LOOP UNTIL mb% = 0
     MouseStatus mb%, mx%, my%
     oldX% = mx%: oldY% = my%
     KILL LineMemFile$ + ".blu"
     KILL LineMemFile$ + ".grn"
     KILL LineMemFile$ + ".red"
     KILL LineMemFile$ + ".int"
  VIEW
END SUB

SUB Tool.Rectangle
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
     LineMemFile$ = "\memscr"
     MouseStatus mb%, mx%, my%
     IF mb% = 1 THEN DrawClr% = ForeClr%
     IF mb% = 2 THEN DrawClr% = BackClr%
     oX% = mx%: oY% = my%
     Mouse Hide
     Save640 LineMemFile$
     Mouse Show
     DO
       MouseStatus mb%, mx%, my%
       IF mx% <> nx% OR my% <> ny% THEN
          Mouse Hide
          Load640 LineMemFile$
          LINE (oX%, oY%)-(mx%, my%), DrawClr%, B
          LINE (oX%, oY%)-(mx%, my%), DrawClr%, B
          Mouse Show
       END IF
       nx% = mx%: ny% = my%
     LOOP UNTIL mb% = 0
     KILL LineMemFile$ + ".blu"
     KILL LineMemFile$ + ".grn"
     KILL LineMemFile$ + ".red"
     KILL LineMemFile$ + ".int"
  VIEW
END SUB

SUB Tool.RoundedBox
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
     LineMemFile$ = "\memscr"
     MouseStatus mb%, mx%, my%
     IF mb% = 1 THEN DrawClr% = ForeClr%
     IF mb% = 2 THEN DrawClr% = BackClr%
     oX% = mx%: oY% = my%
     Mouse Hide
     Save640 LineMemFile$
     Mouse Show
     DO
       MouseStatus mb%, mx%, my%
       IF mx% <> nx% OR my% <> ny% THEN
          Mouse Hide
          Load640 LineMemFile$
          'LINE (ox%, oy%)-(mx%, my%), DrawClr%, B
          'LINE (ox%, oy%)-(mx%, my%), DrawClr%, B
          'RoundBox ox%, oy%, ABS(ox% - mx%), ABS(oy% - my%), DrawClr%, DrawClr%, DrawClr%, DrawClr%
          RoundBox oX%, oY%, ABS(oX% - mx%), ABS(oY% - my%), BackClr%, BackClr%, DrawClr%, BackClr%
          'RoundBox ox%, oy%, ox% - mx%, oy% - my%, BackClr%, BackClr%, DrawClr%, BackClr%
          Mouse Show
       END IF
       nx% = mx%: ny% = my%
     LOOP UNTIL mb% = 0
     KILL LineMemFile$ + ".blu"
     KILL LineMemFile$ + ".grn"
     KILL LineMemFile$ + ".red"
     KILL LineMemFile$ + ".int"
  VIEW
END SUB

REM $DYNAMIC
SUB Tool.Selection
     'MAXIMUM 32767 INT Indexes
     '        Probably  -> Area
     '32698 INT Indexes -> 128160 pix^2
     '31966 INT Indexes -> 126585 pix^2

  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
     'SetMouse HorzLimit, PAX1% + 1, PAX2% - 2
     'SetMouse VertLimit, PAY1% + 1, PAY2% - 2
     ArraySize& = 0
     LineMemFile$ = "\memscr"
     MouseStatus mb%, mx%, my%
     IF mb% = 1 THEN DrawClr% = ForeClr%
     IF mb% = 2 THEN DrawClr% = BackClr%
     oX% = mx%: oY% = my%
     Mouse Hide
     Save640 LineMemFile$
     Mouse Show
     DO
       MouseStatus mb%, mx%, my%
       fx% = mx%: fy% = my%
       IF mx% <> nx% OR my% <> ny% THEN
          Mouse Hide
          IF mx% < PAX1% + 1 THEN SetMouse Position, PAX1% + 1, my%
          IF mx% > PAX2% - 2 THEN SetMouse Position, PAX2% - 2, my%
          IF my% < PAY1% + 1 THEN SetMouse Position, mx%, PAY1% + 1
          IF my% > PAY2% - 2 THEN SetMouse Position, mx%, PAY2% - 2
          Load640 LineMemFile$
          LINE (oX%, oY%)-(fx%, fy%), DrawClr%, B, &HAA00
          LINE (oX%, oY%)-(fx%, fy%), DrawClr%, B, &HAA00
          Mouse Show
          'ArraySize& = ABS(4 + INT(((mx% - oX% + 1) * (1) + 7) / 8) * 4 * ((my% - oY%) + 1))
          'TrueArraySize& = ABS(ArraySize& / 2)
          'IF TrueArraySize& > 32700 THEN EXIT DO
          SelWidth& = ABS((fx% - oX%))
          SelHeight& = ABS((fy% - oY%))
          Area& = SelWidth& * SelHeight&
          IF Area& > 126585 THEN EXIT DO '31966
          'LOCATE 1, 1: PRINT TrueArraySize&, Area&, SelWidth&, SelHeight&
       END IF
       nx% = mx%: ny% = my%
     LOOP UNTIL mb% = 0
     'DIM SelBuff%(TrueArraySize&)
     DIM SelBuff%(32767)
      Mouse Hide
          LINE (oX%, oY%)-(fx%, fy%), ForeClr%, B, &HAA00
          tox% = oX%: toy% = oY%: tfx% = fx%: tfy% = fy%
     DO
       MouseStatus mb%, mx%, my%
       Mouse Show
     LOOP UNTIL mb% = 1
     oX% = tox%: oY% = toy%: fx% = tfx%: fy% = tfy%
     Mouse Hide
     Load640 LineMemFile$
       GET (oX%, oY%)-(fx%, fy%), SelBuff%
       LINE (oX%, oY%)-(fx%, fy%), BackClr%, BF
     Save640 LineMemFile$
      IF fx% < oX% THEN oX% = fx%
      IF fy% < oY% THEN oY% = fy%
     
      MouseStatus mb%, mx%, my%

      XAlt% = oX%: YAlt% = oY%: Newmx% = mx%: Newmy% = my%
      MoveX% = (Newmx% - XAlt%): MoveY% = (Newmy% - YAlt%)
     DO
      MouseStatus mb%, mx%, my%
      Mouse Show
      IF mx% <> nx% OR my% <> ny% THEN
       Mouse Hide
        AltMoveX% = mx% - MoveX%: AltMoveY% = my% - MoveY%
        Load640 LineMemFile$
        IF AltMoveX% < PAX1% + 1 THEN AltMoveX% = PAX1% + 1
        IF AltMoveY% < PAY1% + 1 THEN AltMoveY% = PAY1% + 1
        'IF AltMoveX% + SelWidth& > PAX2% - 3 THEN AltMoveX% = PAX2% - AltMoveX% + SelWidth& - 2
        IF AltMoveX% + SelWidth& > PAX2% - 2 THEN AltMoveX% = PAX2% - SelWidth& - 2
        IF AltMoveY% + SelHeight& > PAY2% - 2 THEN AltMoveY% = PAY2% - SelHeight& - 2
         PUT (AltMoveX%, AltMoveY%), SelBuff%, PSET
         LINE (AltMoveX%, AltMoveY%)-(AltMoveX% + SelWidth&, AltMoveY% + SelHeight&), ForeClr%, B, &HAA00
       Mouse Show
      END IF
      nx% = mx%: ny% = my%
     LOOP UNTIL mb% = 0
     Mouse Hide
     Load640 LineMemFile$
      'AltMoveX% = mx% - MoveX%: AltMoveY% = my% - MoveY%
      PUT (AltMoveX%, AltMoveY%), SelBuff%, AND
     Mouse Show
     ERASE SelBuff%
     KILL LineMemFile$ + ".blu"
     KILL LineMemFile$ + ".grn"
     KILL LineMemFile$ + ".red"
     KILL LineMemFile$ + ".int"
  VIEW
END SUB

REM $STATIC
SUB Tool.Text
SHARED TheFontPath
'LayerMode% => 1 - Using a Background Color
'              2 - Keeping Background Layer
LayerMode% = 2
  VIEW SCREEN (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY2% - 2)
     LineMemFile$ = "\memscr"
     MouseStatus mb%, mx%, my%
     IF mb% = 1 THEN DrawClr% = ForeClr%
     IF mb% = 2 THEN DrawClr% = BackClr%
     oX% = mx%: oY% = my%
     Mouse Hide
     Save640 LineMemFile$
     Mouse Show
     DO
       MouseStatus mb%, mx%, my%
       IF mx% <> nx% OR my% <> ny% THEN
          Mouse Hide
          Load640 LineMemFile$
          LINE (oX%, oY%)-(mx%, my%), DrawClr%, B, &HAAAA
          LINE (oX%, oY%)-(mx%, my%), DrawClr%, B, &HAAAA
          IF LayerMode% = 2 THEN
             ArraySize& = ABS(4 + INT(((mx% - oX% + 1) * (1) + 7) / 8) * 4 * ((my% - oY%) + 1))
             TrueArraySize& = ABS(ArraySize& / 2)
             IF TrueArraySize& > 32000 THEN EXIT DO
          END IF
          Mouse Show
       END IF
       nx% = mx%: ny% = my%
     LOOP UNTIL mb% = 0
     Load640 LineMemFile$
     IF LayerMode% = 2 THEN
       DIM SelBuff%(TrueArraySize&)
       GET (oX%, oY%)-(mx%, my%), SelBuff%
     END IF
     Mouse Hide
     LINE (oX%, oY%)-(mx%, my%), BackClr%, BF
     LINE (oX%, oY%)-(mx%, my%), DrawClr%, B, &HAAAA
     Mouse Show
      'ActualFont$ = StdFnt$
      ActualFont$ = TheFontPath$
      ActualFont$ = ActualFont$ + "lucidabl.fnt"
      'ActualFont$ = TheFontPath$ + "tahoma.fnt"
      'Stencil
      'Kids
      'Shotgun
      'Curlz
      'Mac
      'Lucidabl
     MouseStatus mb%, mx%, my%
     theW% = ABS(mx% - oX%)
     theH% = ABS(my% - oY%)
     xpos% = oX%: ypos% = oY%
     oxpos% = oX%: oypos% = oY%
     FullString$ = ""
     CharSep% = 16
     'FButon xpos%, ypos%, theW%, theH%, 7, 15, 0, 8
     DO
       Keyb$ = INKEY$
       SELECT CASE Keyb$
         CASE CHR$(32) TO CHR$(125)
           IF Keyb$ = CHR$(32) THEN IsSpace% = 1
           FullString$ = FullString$ + Keyb$
           xpos0a% = GetMaxStrVal%(FullString$, ActualFont$, 0)
           IF xpos0a% > theW% THEN
              IF ypos% + CharSep% > oypos% + theH% - CharSep% THEN GOTO ToolTextEnd
              IF (Keyb$ <> CHR$(32)) AND IsSpace% = 1 THEN
                  FOR u% = LEN(FullString$) TO 1 STEP -1
                     IF MID$(FullString$, u%, 1) = CHR$(32) THEN EXIT FOR
                  NEXT u%
                  FString$ = MID$(FullString$, 1, u% - 1)
                  FullString$ = RTRIM$(MID$(FullString$, u% + 1, LEN(FullString$) - u% + 1))
                  Mouse Hide
                  FButon xpos% + 1, ypos% + 1, theW% - 2, CharSep% + 1, BackClr%, BackClr%, BackClr%, BackClr%
                  QFont FString$, xpos%, ypos%, ForeClr%, ActualFont$, 0
                  Mouse Show
                  ypos% = ypos% + CharSep%
                  IsSpace% = 0
              ELSE
                 ypos% = ypos% + CharSep%
                 IF Keyb$ = CHR$(32) THEN Keyb$ = ""
                 FullString$ = Keyb$
              END IF
           END IF
           Mouse Hide
           'FButon xpos% + 1, ypos% + 1, theW% - 2, CharSep% + 1, BackClr%, BackClr%, BackClr%, BackClr%
           QFont FullString$, xpos%, ypos%, ForeClr%, ActualFont$, 0
           'QFont FullString$ + ".", xpos%, ypos%, ForeClr%, ActualFont$, 0
           Mouse Show
         CASE CHR$(13)
           EXIT DO
       END SELECT
     LOOP
ToolTextEnd:
     Mouse Hide
     LINE (oX%, oY%)-(mx%, my%), BackClr%, B
     IF LayerMode% = 2 THEN
       PUT (oX%, oY%), SelBuff%, AND
       ERASE SelBuff%
     END IF
     Mouse Show
     KILL LineMemFile$ + ".blu"
     KILL LineMemFile$ + ".grn"
     KILL LineMemFile$ + ".red"
     KILL LineMemFile$ + ".int"
  VIEW
END SUB

SUB ToolsSettingsDraw (Which%)
'********************************************************
'These tools don't need extra settings
'Pencil
'Dropper
'Filler
'********************************************************
'LINE (5, 240)-(55, 320), 1, BF
'FButon 5, 240, 50, 80, 7, 0, 15, 8

'LOCATE 1, 1: PRINT AcTool%
'MouseStatus mb%, mx%, my%
'IF (MSOverArea(5, 240, 55, 320) = 1) AND mb% = 1 THEN
'  SELECT CASE AcTool%
'  CASE 11
'  END SELECT
'END IF

  SELECT CASE Which%
  CASE 1
     FButon 5, 240, 50, 80, 7, 0, 15, 8
  CASE 2
     FButon 5, 240, 50, 80, 7, 0, 15, 8
  CASE 3
     FButon 5, 240, 50, 80, 7, 0, 15, 8
  CASE 4
     FButon 5, 240, 50, 80, 7, 0, 15, 8
  CASE 5
     FButon 5, 240, 50, 80, 7, 0, 15, 8
  CASE 6
     FButon 5, 240, 50, 80, 7, 0, 15, 8
  CASE 7
     FButon 5, 240, 50, 80, 7, 0, 15, 8
  CASE 8
     FButon 5, 240, 50, 80, 7, 0, 15, 8
  CASE 9
     FButon 5, 240, 50, 80, 7, 0, 15, 8
     CONST PI = 3.141592654#
     RANDOMIZE TIMER
     Cons! = PI / 180: x% = 15: y% = 255: MaxRad% = 5: c& = 0
     DO
      c& = c& + 1
        i% = INT(RND * 360): R% = INT(RND * MaxRad%)
        PSET (x% + R% * COS(Cons! * i%), y% - R% * SIN(Cons! * i%)), 0
     LOOP UNTIL c& = 100
     x% = 40: y% = 255: MaxRad% = 10: c& = 0
     DO
      c& = c& + 1
        i% = INT(RND * 360): R% = INT(RND * MaxRad%)
        PSET (x% + R% * COS(Cons! * i%), y% - R% * SIN(Cons! * i%)), 0
     LOOP UNTIL c& = 150
     x% = 25: y% = 290: MaxRad% = 13: c& = 0
     DO
      c& = c& + 1
        i% = INT(RND * 360): R% = INT(RND * MaxRad%)
        PSET (x% + R% * COS(Cons! * i%), y% - R% * SIN(Cons! * i%)), 0
     LOOP UNTIL c& = 200
  CASE 10
     FButon 5, 240, 50, 80, 7, 0, 15, 8
  CASE 11
     FButon 5, 240, 50, 80, 7, 0, 15, 8
     LINE (10, 250)-(10 + 40, 250), 0, BF
     LINE (10, 260)-(10 + 40, 261), 0, BF
     LINE (10, 270)-(10 + 40, 272), 0, BF
  CASE 12
     FButon 5, 240, 50, 80, 7, 0, 15, 8
     LINE (10, 250)-(10 + 40, 250), 0, BF
     LINE (10, 260)-(10 + 40, 261), 0, BF
     LINE (10, 270)-(10 + 40, 272), 0, BF
  CASE 13
     FButon 5, 240, 50, 80, 7, 0, 15, 8
     LINE (10, 250)-(10 + 40, 260), 0, B
      HalfToneButton 10, 265, 40, 10, 7, 8, 0, 0, 0
      'LINE (10, 265)-(10 + 40, 275), 8, BF
      'LINE (10, 265)-(10 + 40, 275), 14, B
     LINE (10, 280)-(10 + 40, 290), 0, BF
  CASE 14
     FButon 5, 240, 50, 80, 7, 0, 15, 8
     LINE (10, 250)-(10 + 40, 260), 0, B
      HalfToneButton 10, 265, 40, 10, 7, 8, 0, 0, 0
     LINE (10, 280)-(10 + 40, 290), 0, BF
  CASE 15
     FButon 5, 240, 50, 80, 7, 0, 15, 8
     LINE (10, 250)-(10 + 40, 260), 0, B
      HalfToneButton 10, 265, 40, 10, 7, 8, 0, 0, 0
     LINE (10, 280)-(10 + 40, 290), 0, BF
  CASE 16
     FButon 5, 240, 50, 80, 7, 0, 15, 8
     LINE (10, 250)-(10 + 40, 260), 0, B
      HalfToneButton 10, 265, 40, 10, 7, 8, 0, 0, 0
     LINE (10, 280)-(10 + 40, 290), 0, BF
  END SELECT

END SUB


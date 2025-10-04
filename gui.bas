'** If you are reading this message you loaded the wrong module
'** It doesn't do anything by itself.
'** PLEASE, In order to run the full application you need to
'** to load QUICKPT4.BAS....
'****** Change the path in line #25 (QB.BI) for your own path
DEFINT A-Z
DECLARE FUNCTION TextBox$ (xpos%, ypos%, W%, h%, Font$, MaxChar%)
DECLARE SUB DrawLabel (WinID%, Obj%, x1%, y1%, W%, h%, Caption$)
DECLARE SUB KillDialog (ID%)
DECLARE FUNCTION DrawButton% (Win%, Obj%, x1%, y1%, W%, h%, Caption$)
DECLARE SUB FButon (x1%, y1%, W%, h%, bck%, light%, shadow%, shadow2%)
DECLARE FUNCTION MenuBar% (ItemList$(), NoOfItems%, X%, y%, MenuID%)
DECLARE FUNCTION GetMaxStrVal% (Txt$, FontFile$, Attribs%)
DECLARE FUNCTION MSOverArea% (x1%, y1%, x2%, y2%)
DECLARE SUB DrawDialog (ID%, X%, y%, W%, h%, Caption$)
DECLARE SUB Save640 (File$)
DECLARE SUB Load640 (File$)
DECLARE SUB MouseStatus (mb%, X%, y%)
DECLARE SUB Mouse (Action%)
DECLARE SUB ReDrawObjects ()
DECLARE SUB QFont (Txt$, X%, y%, C%, FontFile$, Attribs%)
DECLARE FUNCTION CheckBit% (BYTE AS STRING, WhichBit AS INTEGER)
DECLARE FUNCTION ChangeBit$ (BYTE AS STRING, WhichBit AS INTEGER, ChangeTo AS INTEGER)
DECLARE FUNCTION CurrDir$ (Dr$)
'$INCLUDE: 'qb.bi'
TYPE FontCharInfo
	 CharWidth AS INTEGER
	 CharHeight AS INTEGER
	 FileOffset AS LONG
END TYPE
TYPE DataStructure
 X AS INTEGER: y AS INTEGER: W AS INTEGER: h AS INTEGER
 WinID AS INTEGER: ObjectID AS INTEGER: Options AS STRING * 1: Caption AS STRING * 32
END TYPE
COMMON SHARED PAX1%, PAY1%, PAX2%, PAY2% 'Paint Area Coords
COMMON SHARED BackClr%, ForeClr%, TheImgName$, FileModified%
COMMON SHARED TheFontPath$, StdFnt$, WinFnt$, SwapVideoFile$
DIM SHARED Regs AS RegTypeX, Reg AS RegType
DIM SHARED mb, mx, my, OnTop, NoOfWin
CONST Show = 1, Hide = 2 'Constants for Mouse SUB
CONST MAXWIN = 2, MAXBTN = 5, MAXMENUS = 8, MAXITEMS = 9, MAXOPT = 16
CONST MAXLBL = 5
DIM SHARED Items(MAXMENUS) AS INTEGER, ItemCaption$(MAXMENUS, MAXITEMS)
DIM SHARED Win(MAXWIN) AS DataStructure
DIM SHARED Btn(MAXWIN, MAXBTN) AS DataStructure
DIM SHARED Lbl(MAXWIN, MAXBTN) AS DataStructure
'**************************************************************************

END

Target:
DATA 01,01
DATA F39FE10FF11E7BBC7FFC7FFC8FE207C18FE27FFC7FFC7BBCF11EE10FF39FFFFF00000C6004400001000100010000701C000000010001000104400C6000000000
Arrow:
DATA 03,04
DATA FF1FFF2FFF17FF2BFF35FF3A7F3DBF3E1F3C0F281F107F207F003F303FF03FF800C000A00090008800840082008180804080E083009200A900C9800480048003

SUB AddItem2Menu (MenuID%, ItemID%, Caption$)
 ItemCaption$(MenuID%, ItemID%) = Caption$
 Items(MenuID%) = Items(MenuID%) + 1
END SUB

FUNCTION ChangeBit$ (BYTE AS STRING, WhichBit AS INTEGER, ChangeTo AS INTEGER)
 DIM TheByte AS STRING * 1
 TheByte = BYTE
 FOR i% = 0 TO 7
	 IF ASC(TheByte) AND (2 ^ i%) THEN Suma% = Suma% + (2 ^ i%)
 NEXT i%
 IF (ASC(TheByte) AND (2 ^ (WhichBit - 1))) <> 0 AND ChangeTo = 0 THEN
		Suma% = Suma% - (2 ^ (WhichBit - 1))
 END IF
 IF (ASC(TheByte) AND (2 ^ (WhichBit - 1))) = 0 AND ChangeTo = 1 THEN
		Suma% = Suma% + (2 ^ (WhichBit - 1))
 END IF
 ChangeBit$ = CHR$(Suma%)
END FUNCTION

FUNCTION CheckBit% (BYTE AS STRING, WhichBit AS INTEGER)
 DIM TheByte AS STRING * 1
 TheByte = BYTE
 IF ASC(TheByte) AND (2 ^ (WhichBit - 1)) THEN CheckBit% = 1 ELSE CheckBit% = 0
END FUNCTION

SUB DoMenuBar (ItemList$(), NoOfItems%, X%, y%)
 FOR i% = 1 TO NoOfItems%
		L1% = GetMaxStrVal%(ItemList$(i%), StdFnt$, 0)
		QFont ItemList$(i%), X% + x1%, y%, 0, StdFnt$, 0
		x1% = x1% + L1% + 12
 NEXT i%
END SUB

SUB DrawDialog (ID%, X%, y%, W%, h%, Caption$)
	IF (CheckBit%(Win(ID%).Options, 4) = 0) THEN
		Win(ID%).X = X%: Win(ID%).y = y%: Win(ID%).W = W%: Win(ID%).h = h%
		Win(ID%).Caption = Caption$: Win(ID%).WinID = ID%: Win(ID%).ObjectID = ID%
		Win(ID%).Options = ChangeBit$(Win(ID%).Options, 2, 1) 'Window visible = true
		Win(ID%).Options = ChangeBit$(Win(ID%).Options, 3, 1) 'Window active = true
		Win(ID%).Options = ChangeBit$(Win(ID%).Options, 4, 1) 'Window created = true
		OnTop = ID%
		NoOfWin = NoOfWin + 1
	END IF
BarSep% = 19 'Height of the Title Bar
IF (CheckBit%(Win(ID%).Options, 1) = 0) THEN
WCB = 7: WCL = 15: WCS = 0: WCS2 = 8: WCTB = 1: WCTTC = 15
Mouse Hide
 LINE (X%, y%)-(X% + W%, y%), WCB
 LINE (X%, y% + 1)-(X% + W%, y% + 1), WCL
 LINE (X%, y% + 2)-(X% + W%, y% + 3), WCB, B
 LINE (X%, y%)-(X%, y% + h%), WCB
 LINE (X% + 1, y% + 1)-(X% + 1, y% + h%), WCL
 LINE (X% + 2, y% + 2)-(X% + 3, y% + h%), WCB, B
 LINE (X%, y% + h%)-(X% + W%, y% + h%), WCS
 LINE (X% + 1, y% + h% - 1)-(X% + W%, y% + h% - 1), WCS2
 LINE (X% + 2, y% + h% - 2)-(X% + W%, y% + h% - 3), WCB, B
 LINE (X% + W%, y%)-(X% + W%, y% + h%), WCS
 LINE (X% + W% - 1, y% + 1)-(X% + W% - 1, y% + h% - 1), WCS2
 LINE (X% + W% - 2, y% + 2)-(X% + W% - 3, y% + h% - 2), WCB, B
 '******* Inicio del �rea de Trabajo
	LINE (X% + 4, y% + 4)-(X% + W% - 4, y% + h% - 4), WCB, BF 'Este es el bck
 '******* Fin del �rea de Trabajo
 IF OnTop = ID% THEN CTB% = WCTB ELSE CTB% = WCS2
 LINE (X% + 4, y% + 4)-(X% + W% - 4, y% + BarSep%), CTB%, BF
 'LINE (x% + 4, y% + 4)-(x% + w% - 4, y% + BarSep%), CTB%, B
 'LINE (x% + 4, y% + BarSep%)-(x% + w% - 4, y% + BarSep%), CTB%
 'LINE (x% + w% - 4, y% + BarSep%)-(x% + w% - 4, y% + 4), CTB%
 QFont Caption$, X% + 6, y% + 3, WCTTC, WinFnt$, 0
Mouse Show
 '******************************************************************************
 Win(ID%).Options = ChangeBit$(Win(ID%).Options, 1, 1) 'Window has already drawn
END IF 'Window draws if it hasn't been drawn

MouseStatus mb, mx, my
Mouse Show
 IF (mx > Win(ID%).X + 4 AND my > Win(ID%).y + 4 AND mx < Win(ID%).X + Win(ID%).W - 4 AND my < Win(ID%).y + BarSep%) AND mb = 1 THEN
		Mouse Hide
			MouseStatus mb%, mx%, my%
			n1x% = mx%: n1y% = my%
			XAlt% = Win(ID%).X: YAlt% = Win(ID%).y: Newmx% = mx%: Newmy% = my%
			MoveX% = (Newmx% - XAlt%): MoveY% = (Newmy% - YAlt%)
			 Save640 SwapVideoFile$ + "2"
			 DO
				 Mouse Show
				 MouseStatus mb%, mx%, my%
				 IF mx% <> nx% OR my% <> ny% THEN
				 Mouse Hide
						LINE (mx% - MoveX%, my% - MoveY%)-((mx% - MoveX%) + Win(ID%).W, (my% - MoveY%) + Win(ID%).h), 15, B, &HAAAA
						Load640 SwapVideoFile$ + "2"
				 Mouse Show
				 ELSE
					Mouse Hide
					AltMoveX% = mx% - MoveX%: AltMoveY% = my% - MoveY%
					LINE (mx% - MoveX%, my% - MoveY%)-((mx% - MoveX%) + Win(ID%).W, (my% - MoveY%) + Win(ID%).h), 15, B, &HAAAA
					DO
					 LINE (mx% - MoveX%, my% - MoveY%)-((mx% - MoveX%) + Win(ID%).W, (my% - MoveY%) + Win(ID%).h), 15, B, &HAAAA
					 Mouse Show
					 MouseStatus mb%, mx%, my%
					LOOP UNTIL mx% <> nx% OR my% <> ny% OR mb% = 0
					Mouse Hide
					 Load640 SwapVideoFile$ + "2"
					Mouse Show
				 END IF
				 nx% = mx%: ny% = my%
			 LOOP UNTIL mb% = 0
			Mouse Hide
			 IF mx% <> n1x% OR my% <> n1y% THEN Load640 SwapVideoFile$
			 Load640 SwapVideoFile$
			 Win(ID%).X = mx% - MoveX%: Win(ID%).y = my% - MoveY%
			 OnTop = ID%
			 ReDrawObjects
			 Mouse Show
			 KILL SwapVideoFile$ + "2" + ".BLU"
			 KILL SwapVideoFile$ + "2" + ".GRN"
			 KILL SwapVideoFile$ + "2" + ".RED"
			 KILL SwapVideoFile$ + "2" + ".INT"
 END IF
END SUB

SUB FButon (x1%, y1%, W%, h%, bck%, light%, shadow%, shadow2%)
 'Flat Button
 LINE (x1%, y1%)-(x1% + W%, y1% + h%), bck%, BF
 LINE (x1%, y1%)-(x1% + W%, y1% + h%), light%, B
 LINE (x1% + W%, y1% + h%)-(x1% + W%, y1%), shadow%, B
 LINE (x1% + W%, y1% + h%)-(x1%, y1% + h%), shadow%, B
 LINE (x1% + 1, y1% + h% - 1)-(x1% + W% - 1, y1% + h% - 1), shadow2%
 LINE (x1% + W% - 1, y1% + 1)-(x1% + W% - 1, y1% + h% - 1), shadow2%
END SUB

FUNCTION GetMaxStrVal% (Txt$, FontFile$, Attribs%)
ATTRIB.BOLD = 1: ATTRIB.UNDERLINE = 2: ATTRIB.ITALICS = 4
CONST ALIGN.CENTER = -1: ALIGN.RIGHT = -2
CONST WINFNTQB.VERSIONMAJOR = 1: WINFNTQB.VERSIONMINOR = 0
'**************************************************************************
SHARED TotalStrVal%
undersepar% = 2
DIM Char AS FontCharInfo
OrgAttribs% = Attribs%
A% = INSTR(FontFile$, ".")
IF A% = 0 THEN FontFile$ = FontFile$ + ".fnt"
handle = FREEFILE
OPEN Path$ + FontFile$ FOR BINARY AS #handle
IF LOF(handle) = 0 THEN
CLOSE #handle
KILL FontFile$
EXIT FUNCTION
END IF
Version% = 0
GET #handle, , Version%
IF (Version% MOD 256 <> WINFNTQB.VERSIONMINOR) OR (Version% \ 256 <> WINFNTQB.VERSIONMAJOR) THEN
CLOSE #handle
EXIT FUNCTION
END IF
Ty& = y%
IF X% = ALIGN.CENTER THEN
Tx& = 0
Widest& = 0
FixLR% = 0
FOR i% = 1 TO LEN(Txt$)
CharCnt% = ASC(MID$(Txt$, i%, 1)) - 32
IF CharCnt% = -19 THEN
IF Tx& > Widest& THEN Widest& = Tx&: Tx& = 0
FixLR% = 1
ELSEIF CharCnt% = ASC("$") - 32 THEN
i% = i% + 1
code$ = LCASE$(MID$(Txt$, i%, 1))
IF code$ = "$" THEN
CharCnt% = ASC("$") - 32
GOTO CountCharWidthForCentering2:
END IF
ELSE
CountCharWidthForCentering2:
SEEK #handle, (2 + 2 + 4) * CharCnt% + 1 + 2
GET #handle, , Char
Tx& = Tx& + Char.CharWidth
END IF
NEXT
IF Tx& > Widest& THEN Widest& = Tx&
Tx& = (ScreenResX \ 2) - (Tx& \ 2)
ELSEIF X% = ALIGN.RIGHT THEN
Tx& = 0
Widest& = 0
FixLR% = 0
FOR i% = 1 TO LEN(Txt$)
CharCnt% = ASC(MID$(Txt$, i%, 1)) - 32
IF CharCnt% = -19 THEN
IF Tx& > Widest& THEN Widest& = Tx&: Tx& = 0
FixLR% = 1
ELSEIF CharCnt% = ASC("$") - 32 THEN
i% = i% + 1
code$ = LCASE$(MID$(Txt$, i%, 1))
IF code$ = "$" THEN
CharCnt% = ASC("$") - 32
GOTO CountCharWidthForRight2:
END IF
ELSE
CountCharWidthForRight2:
SEEK #handle, (2 + 2 + 4) * CharCnt% + 1 + 2
GET #handle, , Char
Tx& = Tx& + Char.CharWidth
END IF
NEXT
IF Tx& > Widest& THEN Widest& = Tx&
Tx& = ScreenResX - Tx&
ELSE
Tx& = X%
END IF
ttx& = Tx&
FOR i% = 1 TO LEN(Txt$)
CharCnt% = ASC(MID$(Txt$, i%, 1))
FixLR% = 0
IF CharCnt% = 13 THEN
FixLR% = 1
CharCnt% = ASC("A") - 32
ELSEIF CharCnt% = ASC("$") THEN
i% = i% + 1
code$ = LCASE$(MID$(Txt$, i%, 1))
IF code$ = "b" THEN GOSUB ToggleBold2: CharCnt% = -1
IF code$ = "u" THEN GOSUB ToggleUnderline2: CharCnt% = -1
IF code$ = "i" THEN GOSUB ToggleItalics2: CharCnt% = -1
IF code$ = "c" THEN GOSUB SetTempColor2: CharCnt% = -1
IF code$ = "$" THEN
CharCnt% = ASC("$") - 32
END IF
ELSEIF CharCnt% < 32 OR CharCnt% > 126 THEN
CharCnt% = 127
ELSE
CharCnt% = CharCnt% - 32
END IF
IF CharCnt% > -1 THEN
SEEK #handle, (2 + 2 + 4) * CharCnt% + 1 + 2
GET #handle, , Char
SEEK #handle, Char.FileOffset
REDIM CharDat&(Char.CharHeight)
IF FixLR% <> 0 THEN
ttx& = Tx&
Ty& = Ty& + Char.CharHeight
FixLR% = 0
ELSE
IF CharCnt% <> 0 THEN
FOR cty% = 0 TO Char.CharHeight - 1
t% = 0
GET #handle, , t%
CharDat&(cty%) = t%
Offset% = 0
IF (Attribs% AND ATTRIB.ITALICS) <> 0 THEN Offset% = -cty% / 3
'                      Future.Line ttx& + Offset%, Ty& + cty%, ttx& + 16 + Offset%, Ty& + cty%, C&, CharDat&(cty%)
				 WordWrap.MaxLen = ttx& + Char.CharWidth
				 TotalStrVal% = WordWrap.MaxLen
				 GetMaxStrVal% = TotalStrVal%
				 'Future.Line 0, 20, WordWrap.MaxLen, 20, C&, -1
				 'Future.Print 0, 30, STR$(TotalStrVal&), c15&, c0&
				 'Future.Line ttx&, Char.CharHeight + (-undersepar%) + Ty&, WordWrap.MaxLen, Char.CharHeight + (-undersepar%) + Ty&, c14&, -1
	

IF (Attribs% AND ATTRIB.BOLD) <> 0 THEN
'                      Future.Line ttx& + Offset% + 1, Ty& + cty%, ttx& + 17 + Offset%, Ty& + cty%, C&, CharDat&(cty%)
				 WordWrap.MaxLen = ttx& + Char.CharWidth
				 TotalStrVal% = WordWrap.MaxLen
				 GetMaxStrVal% = TotalStrVal%


END IF
NEXT
END IF
IF (Attribs% AND ATTRIB.UNDERLINE) <> 0 THEN PRINT ""'Future.Line ttx&, Char.CharHeight + (-undersepar%) + Ty&, ttx& + Char.CharWidth, Char.CharHeight + (-undersepar%) + Ty&, C&, -1
				 WordWrap.MaxLen = ttx& + Char.CharWidth
				 TotalStrVal% = WordWrap.MaxLen
				 GetMaxStrVal% = TotalStrVal%
ttx& = ttx& + Char.CharWidth
END IF
END IF
NEXT
CLOSE #handle
Attribs% = OrgAttribs%
EXIT FUNCTION
ToggleBold2:
IF (Attribs% AND ATTRIB.BOLD) <> 0 THEN
Attribs% = Attribs% - ATTRIB.BOLD
ELSE
Attribs% = Attribs% + ATTRIB.BOLD
END IF
RETURN
ToggleUnderline2:
IF (Attribs% AND ATTRIB.UNDERLINE) <> 0 THEN
Attribs% = Attribs% - ATTRIB.UNDERLINE
ELSE
Attribs% = Attribs% + ATTRIB.UNDERLINE
END IF
RETURN
ToggleItalics2:
IF (Attribs% AND ATTRIB.ITALICS) <> 0 THEN
Attribs% = Attribs% - ATTRIB.ITALICS
ELSE
Attribs% = Attribs% + ATTRIB.ITALICS
END IF
RETURN
SetTempColor2:
RETURN
END FUNCTION

SUB Load640 (File$)
 DEF SEG = &HA000
 OUT &H3C4, 2: OUT &H3C5, 1: BLOAD File$ + ".BLU", 0      'bitplane 0
 OUT &H3C4, 2: OUT &H3C5, 2: BLOAD File$ + ".GRN", 0      'bitplane 1
 OUT &H3C4, 2: OUT &H3C5, 4: BLOAD File$ + ".RED", 0      'bitplane 2
 OUT &H3C4, 2: OUT &H3C5, 8: BLOAD File$ + ".INT", 0      'bitplane 3
 OUT &H3C4, 2: OUT &H3C5, 16
 DEF SEG
END SUB

SUB LoadMouseCur (WhichCur%)
SELECT CASE WhichCur%
CASE 1
	RESTORE Arrow
CASE 2
	RESTORE Target
END SELECT
	 READ HSX
	 READ HSY
	 READ cdata$
	 FOR i% = 1 TO LEN(cdata$) STEP 2
				DByte$ = MID$(cdata$, i%, 2)
				cd0$ = "&H" + DByte$
				cd1 = VAL(cd0$)
				cd2$ = CHR$(cd1)
				CursorData$ = CursorData$ + cd2$
	 NEXT i%
	 Reg.ax = 9
	 Reg.bx = HSX
	 Reg.cx = HSY
	 Reg.dx = SADD(CursorData$)
	 INTERRUPT &H33, Reg, Reg
END SUB

FUNCTION MenuBar% (ItemList$(), NoOfItems%, X%, y%, MenuID%)
DEF SEG = &H40
	IF (PEEK(&H17) AND 8) <> 0 THEN
				DO
					IF (PEEK(&H17) AND 8) = 0 THEN EXIT DO
					SelMenu% = 1
				LOOP
				Mouse Hide
				L1% = GetMaxStrVal%(ItemList$(SelMenu%), StdFnt$, 0)
				FButon X% - 5, y%, L1% + 10, 17, 7, 0, 15, 7
				QFont ItemList$(SelMenu%), X% + 1, y% + 1, 0, StdFnt$, 0
				Mouse Show
				DO
					MouseStatus mb%, mx%, my%
					KEYB$ = INKEY$
					SELECT CASE KEYB$
					CASE CHR$(27)
						 EXIT DO
					CASE CHR$(13), CHR$(32), CHR$(0) + "H", CHR$(0) + "P"
						xL1% = x1%: ID% = SelMenu%: MenuID% = ID%
						xi% = X% + x1% - 10: yi% = y% + 21: GOTO ShowMenuSub
					CASE CHR$(0) + "M" 'Right...
						Mouse Hide
						x1% = 0: L1% = 0
						FOR k% = 1 TO SelMenu%
							 L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
							 IF k% = SelMenu% THEN
								 FButon X% + x1% - 5, y%, L1% + 10, 17, 7, 7, 7, 7
								 QFont ItemList$(SelMenu%), X% + x1%, y%, 0, StdFnt$, 0
							 END IF
							 x1% = x1% + L1% + 12
						NEXT k%
						'Mouse Show
						SelMenu% = SelMenu% + 1
						IF SelMenu% > NoOfItems% THEN SelMenu% = 1
						x1% = 0: L1% = 0
						'Mouse Hide
						FOR k% = 1 TO SelMenu%
							 L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
							 IF k% = SelMenu% THEN
								 FButon X% + x1% - 5, y%, L1% + 10, 17, 7, 0, 15, 7
								 QFont ItemList$(SelMenu%), X% + x1% + 1, y% + 1, 0, StdFnt$, 0
							 END IF
							 x1% = x1% + L1% + 12
						NEXT k%
						'Mouse Show
						x1% = 0: L1% = 0 'This following FOR loop is to get the x1%, L1% Coordinate...
						'Mouse Hide
						FOR k% = 1 TO SelMenu% - 1
							 L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
							 IF k% = SelMenu% THEN
								 FButon X% + x1% - 5, y%, L1% + 10, 17, 7, 0, 15, 7
								 QFont ItemList$(SelMenu%), X% + x1% + 1, y% + 1, 0, StdFnt$, 0
							 END IF
							 x1% = x1% + L1% + 12
						NEXT k%
						Mouse Show
					CASE CHR$(0) + "K" 'Left...
						Mouse Hide
						x1% = 0: L1% = 0
						FOR k% = 1 TO SelMenu%
							 L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
							 IF k% = SelMenu% THEN
								 FButon X% + x1% - 5, y%, L1% + 10, 17, 7, 7, 7, 7
								 QFont ItemList$(SelMenu%), X% + x1%, y%, 0, StdFnt$, 0
							 END IF
							 x1% = x1% + L1% + 12
						NEXT k%
						'Mouse Show
						SelMenu% = SelMenu% - 1
						IF SelMenu% < 1 THEN SelMenu% = NoOfItems%
						x1% = 0: L1% = 0
						'Mouse Hide
						FOR k% = 1 TO SelMenu%
							 L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
							 IF k% = SelMenu% THEN
								 FButon X% + x1% - 5, y%, L1% + 10, 17, 7, 0, 15, 7
								 QFont ItemList$(SelMenu%), X% + x1% + 1, y% + 1, 0, StdFnt$, 0
							 END IF
							 x1% = x1% + L1% + 12
						NEXT k%
						'Mouse Show
						x1% = 0: L1% = 0 'This following FOR loop is to get the x1%, L1% Coordinate...
						'Mouse Hide
						FOR k% = 1 TO SelMenu% - 1
							 L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
							 IF k% = SelMenu% THEN
								 FButon X% + x1% - 5, y%, L1% + 10, 17, 7, 0, 15, 7
								 QFont ItemList$(SelMenu%), X% + x1% + 1, y% + 1, 0, StdFnt$, 0
							 END IF
							 x1% = x1% + L1% + 12
						NEXT k%
						Mouse Show
					END SELECT
					IF mb% = 1 THEN EXIT DO
					IF (PEEK(&H17) AND 8) <> 0 THEN
						DO
							IF (PEEK(&H17) AND 8) = 0 THEN EXIT DO
						LOOP
						EXIT DO
					END IF
				LOOP
				x1% = 0: L1% = 0 'This is to erease the draw
				FOR k% = 1 TO SelMenu%
						L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
						IF k% = SelMenu% THEN
							FButon X% + x1% - 5, y%, L1% + 10, 17, 7, 7, 7, 7
							QFont ItemList$(SelMenu%), X% + x1%, y%, 0, StdFnt$, 0
						END IF
						x1% = x1% + L1% + 12
				NEXT k%
	END IF

	x1% = 0: L1% = 0
	FOR i% = 1 TO NoOfItems%
		 L1% = GetMaxStrVal%(ItemList$(i%), StdFnt$, 0)
		 IF MSOverArea%(X% + x1% - 6, y%, X% + x1% + L1% + 6, y% + 17) = 1 THEN
			 MenuID% = i%
			 SelMenu% = MenuID%
			 Mouse Hide
				FButon X% + x1% - 5, y%, L1% + 10, 17, 7, 15, 0, 7
				QFont ItemList$(i%), X% + x1% - 1, y% - 1, 0, StdFnt$, 0
			 Mouse Show
			 DO
				 MouseStatus mb%, mx%, my%
				 KEYB$ = INKEY$ 'To clean the Keyboard buffer...
				 'SELECT CASE KEYB$
				 'CASE CHR$(13), CHR$(32), CHR$(0) + "H", CHR$(0) + "P"
				 '   Mouse Hide
				 '   FButon x% + x1% - 5, y%, L1% + 10, 17, 7, 0, 15, 7
				 '   QFont ItemList$(i%), x% + x1% + 1, y% + 1, 0, StdFnt$, 0
				 '   Mouse Show
				 '   xL1% = x1%
				 '   ID% = i%: xi% = x% + x1% - 10: yi% = y% + 21: GOTO ShowMenuSub
				 'END SELECT
				 IF (PEEK(&H17) AND 8) <> 0 THEN EXIT DO
				 IF mb% = 1 THEN
						Mouse Hide
						FButon X% + x1% - 5, y%, L1% + 10, 17, 7, 0, 15, 7
						QFont ItemList$(i%), X% + x1% + 1, y% + 1, 0, StdFnt$, 0
						Mouse Show
						DO
							MouseStatus mb%, mx%, my%
						LOOP UNTIL mb% = 0
						xL1% = x1%
						ID% = i%: xi% = X% + x1% - 10: yi% = y% + 21: GOTO ShowMenuSub
				 END IF
			 LOOP UNTIL MSOverArea%(X% + x1% - 6, y%, X% + x1% + L1% + 6, y% + 17) = 0
			 Mouse Hide
				FButon X% + x1% - 5, y%, L1% + 10, 18, 7, 7, 7, 7
				QFont ItemList$(i%), X% + x1%, y%, 0, StdFnt$, 0
			 Mouse Show
		 END IF
		 x1% = x1% + L1% + 12
	NEXT i%

EXIT FUNCTION

ShowMenuSub:
Bckclr% = 7: Selclr% = 1: SelL% = 1: SelS% = 1: SelS2% = 1: SelTxtColr% = 15
TxtAttr% = 0

 IF Items(ID%) > 0 THEN
	CharHeight% = 18
	Sep% = 2
	FOR i% = 1 TO Items(ID%)
		IF LEN(ItemCaption$(ID%, i%)) > LEN(ItemCaption$(ID%, i% - 1)) THEN
		 TWLen% = GetMaxStrVal%(ItemCaption$(ID%, i%), StdFnt$, 0)
		END IF
	NEXT i%
	i% = i% - 1
	THLen% = i% * CharHeight% 'Total Width/Height Lenght

	Mouse Hide
	Save640 "\mem"
	'FOR ji% = 1 TO THLen% + 2
	'  FButon xi%, yi% - 1, TWLen% + 10, ji%, Bckclr%, 15, 0, Bckclr%
	'  Millidelay 3
	'NEXT ji%
	FButon xi%, yi% - 1, TWLen% + 10, THLen% + 2, Bckclr%, 15, 0, Bckclr%

	x1% = xi% + 5: y1% = yi%
	FOR i% = 1 TO Items(ID%)
		QFont ItemCaption$(ID%, i%), x1%, y1%, 0, StdFnt$, 0
		y1% = y1% + CharHeight%
	NEXT i%
	DEF SEG = &H40 'To use ALT Key
MainLoop:
	 itcnt% = 0
	 x1% = xi% + 5: y1% = yi%
	Mouse Show
	DO
		KEYB$ = UCASE$(INKEY$)
		MouseStatus mb%, mx%, my%
		GOSUB CheckKeys
		IF mx% <> nx% OR my% <> ny% THEN
	 
			 FOR i% = 1 TO Items(ID%)
				 y1a% = yi% + (CharHeight% * i%) - CharHeight%
				 y2a% = yi% + (CharHeight% * i%)
				 TWLenA% = GetMaxStrVal%(ItemCaption$(ID%, i%), StdFnt$, 0)
				 IF mx% > x1% AND my% > y1a% AND mx% < x1% + TWLenA% AND my% < y2a% THEN
						IF itcnt% <> 0 THEN
							Mouse Hide
							AuxW% = GetMaxStrVal%(ItemCaption$(ID%, itcnt%), StdFnt$, 0)
							y1% = yi% + (CharHeight% * (itcnt%)) - CharHeight%
							FButon x1%, y1% + Sep%, AuxW%, CharHeight% - (Sep% * 2), Bckclr%, Bckclr%, Bckclr%, Bckclr%
							QFont ItemCaption$(ID%, itcnt%), x1%, y1%, 0, StdFnt$, 0
							Mouse Show
						END IF

						itcnt% = i%
						Mouse Hide
						AuxW% = GetMaxStrVal%(ItemCaption$(ID%, itcnt%), StdFnt$, 0)
						y1% = yi% + (CharHeight% * itcnt%) - CharHeight%
						FButon x1%, y1% + Sep%, AuxW%, CharHeight% - (Sep% * 2), Selclr%, SelL%, SelS%, SelS2%
						QFont ItemCaption$(ID%, itcnt%), x1%, y1%, SelTxtColr%, StdFnt$, TxtAttr%
						Mouse Show
						DO
							KEYB$ = UCASE$(INKEY$)
							MouseStatus mb%, mx%, my%
							IF KEYB$ <> "" THEN GOSUB CheckKeys: EXIT DO
							IF (PEEK(&H17) AND 8) <> 0 THEN
								DO
									IF (PEEK(&H17) AND 8) = 0 THEN EXIT DO
								LOOP
								itcnt% = -1
								GOTO End1
							END IF
							IF mb% <> 0 THEN GOTO End1
							IF mx% < x1% OR my% < y1a% OR mx% > x1% + TWLenA% OR my% > y2a% THEN EXIT DO
						LOOP
				 END IF
			 NEXT i%
		END IF

		IF mb% <> 0 AND (mx% < xi% OR mx% > xi% + TWLen% + 10 OR my% < yi% - 1 OR my% > yi% + THLen% + 2) THEN
		 itcnt% = -1
		 GOTO End1
		END IF
		nx% = mx%: ny% = my%
	LOOP

End1:
i% = itcnt%
IF i% <> -1 THEN
	y1a% = yi% + (CharHeight% * i%) - CharHeight%
	y2a% = yi% + (CharHeight% * i%)
	TWLenA% = GetMaxStrVal%(ItemCaption$(ID%, i%), StdFnt$, 0)
	DO
		 MouseStatus mb%, mx%, my%
		 IF mx% < x1% OR my% < y1a% OR mx% > x1% + TWLenA% OR my% > y2a% THEN
			 itcnt% = -1
			 EXIT DO
		 END IF
	LOOP UNTIL mb% = 0
END IF
End2:
	MenuBar% = itcnt%
	Mouse Hide
	Load640 "\mem"
	KILL "\mem.blu"
	KILL "\mem.grn"
	KILL "\mem.red"
	KILL "\mem.int"
	 L1% = GetMaxStrVal%(ItemList$(ID%), StdFnt$, 0)
	 FButon X% + xL1% - 5, y%, L1% + 10, 18, 7, 7, 7, 7
	 QFont ItemList$(ID%), X% + xL1%, y%, 0, StdFnt$, 0
	Mouse Show
	EXIT FUNCTION
 ELSE
 END IF
'*************************************************************************
CheckKeys:
		IF (PEEK(&H17) AND 8) <> 0 THEN
			 DO
				 IF (PEEK(&H17) AND 8) = 0 THEN EXIT DO
			 LOOP
			 itcnt% = -1
			 GOTO End1
		END IF
		SELECT CASE KEYB$
		CASE CHR$(0) + "H" 'Up
			 Mouse Hide
			 itcnt% = itcnt% - 1
			 IF itcnt% < 1 THEN
					FButon x1%, y1% + Sep%, AuxW%, CharHeight% - (Sep% * 2), Bckclr%, Bckclr%, Bckclr%, Bckclr%
					QFont ItemCaption$(ID%, 1), x1%, y1%, 0, StdFnt$, 0
					itcnt% = Items(ID%)
			 END IF
			 IF itcnt% < Items(ID%) THEN
				 AuxW% = GetMaxStrVal%(ItemCaption$(ID%, itcnt% + 1), StdFnt$, 0)
				 y1% = yi% + (CharHeight% * (itcnt% + 1)) - CharHeight%
				 FButon x1%, y1% + Sep%, AuxW%, CharHeight% - (Sep% * 2), Bckclr%, Bckclr%, Bckclr%, Bckclr%
				 QFont ItemCaption$(ID%, itcnt% + 1), x1%, y1%, 0, StdFnt$, 0
			 END IF
			 AuxW% = GetMaxStrVal%(ItemCaption$(ID%, itcnt%), StdFnt$, 0)
			 y1% = yi% + (CharHeight% * itcnt%) - CharHeight%
			 FButon x1%, y1% + Sep%, AuxW%, CharHeight% - (Sep% * 2), Selclr%, SelL%, SelS%, SelS2%
			 QFont ItemCaption$(ID%, itcnt%), x1%, y1%, SelTxtColr%, StdFnt$, TxtAttr%
			 Mouse Show
		CASE CHR$(0) + "P" 'Down
			 Mouse Hide
			 itcnt% = itcnt% + 1
			 IF itcnt% > Items(ID%) THEN
					y1% = yi% + (CharHeight% * Items(ID%)) - CharHeight%
					FButon x1%, y1% + Sep%, AuxW%, CharHeight% - (Sep% * 2), Bckclr%, Bckclr%, Bckclr%, Bckclr%
					QFont ItemCaption$(ID%, Items(ID%)), x1%, y1%, 0, StdFnt$, 0
					itcnt% = 1
			 END IF
			 IF itcnt% > 1 THEN
				 AuxW% = GetMaxStrVal%(ItemCaption$(ID%, itcnt% - 1), StdFnt$, 0)
				 y1% = yi% + (CharHeight% * (itcnt% - 1)) - CharHeight%
				 FButon x1%, y1% + Sep%, AuxW%, CharHeight% - (Sep% * 2), Bckclr%, Bckclr%, Bckclr%, Bckclr%
				 QFont ItemCaption$(ID%, itcnt% - 1), x1%, y1%, 0, StdFnt$, 0
			 END IF
			 AuxW% = GetMaxStrVal%(ItemCaption$(ID%, itcnt%), StdFnt$, 0)
			 y1% = yi% + (CharHeight% * itcnt%) - CharHeight%
			 FButon x1%, y1% + Sep%, AuxW%, CharHeight% - (Sep% * 2), Selclr%, SelL%, SelS%, SelS2%
			 QFont ItemCaption$(ID%, itcnt%), x1%, y1%, SelTxtColr%, StdFnt$, TxtAttr%
			 Mouse Show
		CASE CHR$(0) + "M" 'Right
			x1% = 0: L1% = 0
			FOR k% = 1 TO ID% - 1
					L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
					x1% = x1% + L1% + 12
			NEXT k%
			xL1a% = x1%: IDa% = ID%
			ID% = ID% + 1
			IF ID% > NoOfItems% THEN ID% = 1
			x1% = 0: L1% = 0
			FOR k% = 1 TO ID% - 1
					L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
					x1% = x1% + L1% + 12
			NEXT k%
			xL1% = x1%
			xi% = X% + x1% - 10: yi% = y% + 21
			MenuBar% = itcnt%
			Mouse Hide
			Load640 "\mem"
			L1a% = GetMaxStrVal%(ItemList$(IDa%), StdFnt$, 0)
			FButon X% + xL1a% - 5, y%, L1a% + 10, 17, 7, 7, 7, 7
			QFont ItemList$(IDa%), X% + xL1a%, y%, 0, StdFnt$, 0
			L1% = GetMaxStrVal%(ItemList$(ID%), StdFnt$, 0)
			FButon X% + xL1% - 5, y%, L1% + 10, 17, 7, 0, 15, 7
			QFont ItemList$(ID%), X% + xL1% + 1, y% + 1, 0, StdFnt$, 0
			Mouse Show
			MenuID% = ID%
			GOTO ShowMenuSub
		CASE CHR$(0) + "K" 'Left
			x1% = 0: L1% = 0
			FOR k% = 1 TO ID% - 1
					L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
					x1% = x1% + L1% + 12
			NEXT k%
			xL1a% = x1%: IDa% = ID%
			ID% = ID% - 1
			IF ID% < 1 THEN ID% = NoOfItems%
			x1% = 0: L1% = 0
			FOR k% = 1 TO ID% - 1
					L1% = GetMaxStrVal%(ItemList$(k%), StdFnt$, 0)
					x1% = x1% + L1% + 12
			NEXT k%
			xL1% = x1%
			xi% = X% + x1% - 10: yi% = y% + 21
			MenuBar% = itcnt%
			Mouse Hide
			Load640 "\mem"
			L1a% = GetMaxStrVal%(ItemList$(IDa%), StdFnt$, 0)
			FButon X% + xL1a% - 5, y%, L1a% + 10, 17, 7, 7, 7, 7
			QFont ItemList$(IDa%), X% + xL1a%, y%, 0, StdFnt$, 0
			L1% = GetMaxStrVal%(ItemList$(ID%), StdFnt$, 0)
			FButon X% + xL1% - 5, y%, L1% + 10, 17, 7, 0, 15, 7
			QFont ItemList$(ID%), X% + xL1% + 1, y% + 1, 0, StdFnt$, 0
			Mouse Show
			MenuID% = ID%
			GOTO ShowMenuSub
		CASE CHR$(13), CHR$(32)
			 GOTO End2
		CASE CHR$(27)
			 itcnt% = -1
			 GOTO End1
		END SELECT
RETURN

EXIT FUNCTION
End3:
RETURN
END FUNCTION

SUB Mouse (Action%)
	SELECT CASE Action%
	CASE 1
		 Reg.ax = 1
		 INTERRUPT &H33, Reg, Reg
	CASE 2
		 Reg.ax = 2
		 INTERRUPT &H33, Reg, Reg
	END SELECT
END SUB

SUB MouseStatus (mb%, mx%, my%)
 Reg.ax = 3
 INTERRUPT &H33, Reg, Reg
 mb% = Reg.bx
 mx% = Reg.cx
 my% = Reg.dx
END SUB

FUNCTION MSOverArea% (x1%, y1%, x2%, y2%)
 MouseStatus mb%, mx%, my%
 IF mx% > x1% AND my% > y1% AND mx% < x2% AND my% < y2% THEN
	 MSOverArea% = 1
 ELSE
	 MSOverArea% = 0
 END IF
END FUNCTION

SUB QFont (Txt$, X%, y%, C%, FontFile$, Attribs%)
ATTRIB.BOLD = 1: ATTRIB.UNDERLINE = 2: ATTRIB.ITALICS = 4
CONST ALIGN.CENTER = -1: ALIGN.RIGHT = -2
CONST WINFNTQB.VERSIONMAJOR = 1: WINFNTQB.VERSIONMINOR = 0
'**************************************************************************
undersepar% = 2
DIM Char AS FontCharInfo
OrgAttribs% = Attribs%
A% = INSTR(FontFile$, ".")
IF A% = 0 THEN FontFile$ = FontFile$ + ".fnt"
handle = FREEFILE
OPEN FontFile$ FOR BINARY AS #handle
IF LOF(handle) = 0 THEN
CLOSE #handle
KILL FontFile$
EXIT SUB
END IF
Version% = 0
GET #handle, , Version%
IF (Version% MOD 256 <> WINFNTQB.VERSIONMINOR) OR (Version% \ 256 <> WINFNTQB.VERSIONMAJOR) THEN
CLOSE #handle
EXIT SUB
END IF
Ty& = y%
IF X% = ALIGN.CENTER THEN
Tx& = 0
Widest& = 0
FixLR% = 0
FOR i% = 1 TO LEN(Txt$)
CharCnt% = ASC(MID$(Txt$, i%, 1)) - 32
IF CharCnt% = -19 THEN
IF Tx& > Widest& THEN Widest& = Tx&: Tx& = 0
FixLR% = 1
ELSEIF CharCnt% = ASC("$") - 32 THEN
i% = i% + 1
code$ = LCASE$(MID$(Txt$, i%, 1))
IF code$ = "$" THEN
CharCnt% = ASC("$") - 32
GOTO CountCharWidthForCentering:
END IF
ELSE
CountCharWidthForCentering:
SEEK #handle, (2 + 2 + 4) * CharCnt% + 1 + 2
GET #handle, , Char
Tx& = Tx& + Char.CharWidth
END IF
NEXT
IF Tx& > Widest& THEN Widest& = Tx&
Tx& = (ScreenResX \ 2) - (Tx& \ 2)
ELSEIF X% = ALIGN.RIGHT THEN
Tx& = 0
Widest& = 0
FixLR% = 0
FOR i% = 1 TO LEN(Txt$)
CharCnt% = ASC(MID$(Txt$, i%, 1)) - 32
IF CharCnt% = -19 THEN
IF Tx& > Widest& THEN Widest& = Tx&: Tx& = 0
FixLR% = 1
ELSEIF CharCnt% = ASC("$") - 32 THEN
i% = i% + 1
code$ = LCASE$(MID$(Txt$, i%, 1))
IF code$ = "$" THEN
CharCnt% = ASC("$") - 32
GOTO CountCharWidthForRight:
END IF
ELSE
CountCharWidthForRight:
SEEK #handle, (2 + 2 + 4) * CharCnt% + 1 + 2
GET #handle, , Char
Tx& = Tx& + Char.CharWidth
END IF
NEXT
IF Tx& > Widest& THEN Widest& = Tx&
Tx& = ScreenResX - Tx&
ELSE
Tx& = X%
END IF
ttx& = Tx&
FOR i% = 1 TO LEN(Txt$)
CharCnt% = ASC(MID$(Txt$, i%, 1))
FixLR% = 0
IF CharCnt% = 13 THEN
FixLR% = 1
CharCnt% = ASC("A") - 32
ELSEIF CharCnt% = ASC("$") THEN
i% = i% + 1
code$ = LCASE$(MID$(Txt$, i%, 1))
IF code$ = "b" THEN GOSUB ToggleBold: CharCnt% = -1
IF code$ = "u" THEN GOSUB ToggleUnderline: CharCnt% = -1
IF code$ = "i" THEN GOSUB ToggleItalics: CharCnt% = -1
IF code$ = "c" THEN GOSUB SetTempColor: CharCnt% = -1
IF code$ = "$" THEN
CharCnt% = ASC("$") - 32
END IF
ELSEIF CharCnt% < 32 OR CharCnt% > 126 THEN
CharCnt% = 127
ELSE
CharCnt% = CharCnt% - 32
END IF
IF CharCnt% > -1 THEN
SEEK #handle, (2 + 2 + 4) * CharCnt% + 1 + 2
GET #handle, , Char
SEEK #handle, Char.FileOffset
REDIM CharDat&(Char.CharHeight)
IF FixLR% <> 0 THEN
ttx& = Tx&
Ty& = Ty& + Char.CharHeight
FixLR% = 0
ELSE
IF CharCnt% <> 0 THEN
FOR cty% = 0 TO Char.CharHeight - 1
t% = 0
GET #handle, , t%
CharDat&(cty%) = t%
Offset% = 0
IF (Attribs% AND ATTRIB.ITALICS) <> 0 THEN Offset% = -cty% / 3
											LINE (ttx& + Offset%, Ty& + cty%)-(ttx& + 16 + Offset%, Ty& + cty%), C%, , CharDat&(cty%)
IF (Attribs% AND ATTRIB.BOLD) <> 0 THEN
											LINE (ttx& + Offset% + 1, Ty& + cty%)-(ttx& + 17 + Offset%, Ty& + cty%), C%, , CharDat&(cty%)
END IF
NEXT
END IF
IF (Attribs% AND ATTRIB.UNDERLINE) <> 0 THEN LINE (ttx&, Char.CharHeight + (-undersepar%) + Ty&)-(ttx& + Char.CharWidth, Char.CharHeight + (-undersepar%) + Ty&), C%
ttx& = ttx& + Char.CharWidth
END IF
END IF
NEXT
CLOSE
Attribs% = OrgAttribs%
EXIT SUB
ToggleBold:
IF (Attribs% AND ATTRIB.BOLD) <> 0 THEN
Attribs% = Attribs% - ATTRIB.BOLD
ELSE
Attribs% = Attribs% + ATTRIB.BOLD
END IF
RETURN
ToggleUnderline:
IF (Attribs% AND ATTRIB.UNDERLINE) <> 0 THEN
Attribs% = Attribs% - ATTRIB.UNDERLINE
ELSE
Attribs% = Attribs% + ATTRIB.UNDERLINE
END IF
RETURN
ToggleItalics:
IF (Attribs% AND ATTRIB.ITALICS) <> 0 THEN
Attribs% = Attribs% - ATTRIB.ITALICS
ELSE
Attribs% = Attribs% + ATTRIB.ITALICS
END IF
RETURN
SetTempColor:
Clr$ = ""
i% = i% + 1
WHILE INSTR("0123456789", MID$(Txt$, i%, 1))
Clr$ = Clr$ + MID$(Txt$, i%, 1)
i% = i% + 1
WEND
i% = i% - 1
C% = VAL(Clr$)
RETURN
END SUB

SUB ReDrawObjects
FOR i% = 0 TO MAXWIN
	IF CheckBit%(Win(i%).Options, 4) = 1 THEN
			Win(i%).Options = ChangeBit$(Win(i%).Options, 1, 0)
			OnTop = i%
			TheCaption$ = RTRIM$(Win(i%).Caption)
			DrawDialog i%, Win(i%).X, Win(i%).y, Win(i%).W, Win(i%).h, TheCaption$
	END IF
NEXT i%
END SUB

SUB Save640 (File$)
 DEF SEG = &HA000
 OUT &H3CE, 4: OUT &H3CF, 0: BSAVE File$ + ".BLU", 0, 38400'bitplane 0 (blue)
 OUT &H3CE, 4: OUT &H3CF, 1: BSAVE File$ + ".GRN", 0, 38400'bitplane 1 (green)
 OUT &H3CE, 4: OUT &H3CF, 2: BSAVE File$ + ".RED", 0, 38400'bitplane 2 (red)
 OUT &H3CE, 4: OUT &H3CF, 3: BSAVE File$ + ".INT", 0, 38400'bitplane 3 (intens.)
 OUT &H3CE, 4: OUT &H3CF, 0
 DEF SEG
END SUB

SUB SetMouse (Action%, var1%, var2%)
'CONST Position = 4, HorzLimit = 7, VertLimit = 8
SELECT CASE Action%
CASE 4 'Position. Const POSITION
	Reg.ax = 4
	Reg.cx = var1%
	Reg.dx = var2%
	INTERRUPT &H33, Reg, Reg
CASE 7 'Horizontal Limit. Const HORZLIMIT
	Reg.ax = 7
	Reg.cx = var1%
	Reg.dx = var2%
	INTERRUPT &H33, Reg, Reg
CASE 8 'Vertical Limit. Const VERTLIMIT
	Reg.ax = 8
	Reg.cx = var1%
	Reg.dx = var2%
	INTERRUPT &H33, Reg, Reg
END SELECT
END SUB

FUNCTION TextBox$ (xpos%, ypos%, W%, h%, Font$, MaxChar%)
F% = 15: L% = 0: S% = 15: S2% = 8: Txt% = 0: Atr% = 0

	FButon xpos%, ypos%, W%, h%, F%, L%, S%, S2%
	x1% = xpos% + 2: y1% = ypos% + 1
	X% = x1%: y% = y1%
	C% = 0
	QFont St2$ + "|", X%, y%, Txt%, Font$, Atr%
	DO
		A$ = INKEY$
		SELECT CASE A$
			CASE CHR$(8)
			IF C% > 0 THEN
				 C% = C% - 1
				 St1$ = MID$(St1$, 1, LEN(St1$) - 1)
				 IF GetMaxStrVal%(St1$, Font$, Atr%) > W% - 16 THEN
						DO
							 E% = E% + 1
							 St2$ = RIGHT$(St1$, E%)
						LOOP UNTIL GetMaxStrVal%(St2$, Font$, Atr%) > W% - 16
				 END IF
				 IF GetMaxStrVal%(St1$, Font$, Atr%) < W% - 8 THEN
							 St2$ = St1$
				 END IF
				 FButon xpos%, ypos%, W%, h%, F%, L%, S%, S2%
				 QFont St2$ + "|", X%, y%, Txt%, Font$, Atr%
				 E% = 0
			END IF
			CASE CHR$(27)
				TextBox$ = "ERROR!"
				EXIT DO
			CASE CHR$(13)
				TextBox$ = St1$
				EXIT FUNCTION
			CASE CHR$(32) TO CHR$(128)
			IF C% < MaxChar% THEN
				St1$ = St1$ + A$
				C% = C% + 1
				IF GetMaxStrVal%(St1$, Font$, Atr%) < W% THEN
					St2$ = St1$
				END IF
				IF GetMaxStrVal%(St1$, Font$, Atr%) > W% - 16 THEN
						 D% = D% + 1
						 FButon xpos%, ypos%, W%, h%, F%, L%, S%, S2%
						 St2$ = MID$(St1$, D%, LEN(St1$))
						 IF GetMaxStrVal%(St2$, Font$, Atr%) > W% - 8 THEN
								 DO
										D% = D% + 1
										FButon xpos%, ypos%, W%, h%, F%, L%, S%, S2%
										St2$ = MID$(St1$, D%, LEN(St1$))
								 LOOP UNTIL GetMaxStrVal%(St2$, Font$, Atr%) < W% - 8
						 END IF
						 IF GetMaxStrVal%(St2$, Font$, Atr%) < W% - 16 THEN
								 DO
										D% = D% - 1
										FButon xpos%, ypos%, W%, h%, F%, L%, S%, S2%
										St2$ = MID$(St1$, D%, LEN(St1$))
								 LOOP UNTIL GetMaxStrVal%(St2$, Font$, Atr%) > W% - 16
						 END IF
				END IF
				D% = 0
				FButon xpos%, ypos%, W%, h%, F%, L%, S%, S2%
				QFont St2$ + "|", X%, y%, Txt%, Font$, Atr%
			END IF
		END SELECT
	LOOP
END FUNCTION


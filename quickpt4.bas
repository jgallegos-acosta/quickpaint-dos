'################## Quick Paint v. Alpha .04 Build 102501 ##################
'* A Paint Program for QuickBasic 4.5 by Jonathan Gallegos
'***************************************************************************
'* First of all, I would like to thank to:
'*  Josh Heaton <josh_heaton@hotmail.com>    -> QFont SUB (FONT was the original name)
'*  Aaron Zabudsky <zabudsk@ecf.utoronto.ca> -> SaveBMP SUB
'*  Jim Emptage <75504.2526@compuserve.com>  -> Bezier Source used in Tool.Curve SUB
'*
'*  My e-mail:  linuxloader815@gmail.com
'*
'*  You can also give your comments, questions, ideas or complains...
'** If you attempt to use part of this code you must write "By JG" somewhere
'** in your own code...

DEFINT A-Z
DECLARE SUB ToolsSettingsDraw (Which%)
DECLARE SUB LoadMouseCur (WhichCur%)
DECLARE SUB Tool.RoundedBox ()
DECLARE SUB Item.SaveAs ()
DECLARE SUB Tool.Selection ()
DECLARE SUB Sub2Quit ()
DECLARE SUB Load640 (FILE$)
DECLARE SUB Item.Undo ()
DECLARE SUB GetUndoMem ()
DECLARE SUB PutUndoMem ()
DECLARE SUB Item.Open ()
DECLARE SUB Item.Settings ()
DECLARE SUB Tool.Brush ()
DECLARE SUB Tool.Text ()
DECLARE FUNCTION CheckBit% (BYTE AS STRING, WhichBit AS INTEGER)
DECLARE FUNCTION ChangeBit$ (BYTE AS STRING, WhichBit AS INTEGER, ChangeTo AS INTEGER)
DECLARE FUNCTION MSOverArea% (x1%, y1%, x2%, y2%)
DECLARE FUNCTION MenuBar% (ItemList$(), NoOfItems%, x%, y%, MenuID%)
DECLARE FUNCTION CurrDir$ (Dr$)
DECLARE SUB InfoBar (InfoFor$, InfoID%)
DECLARE SUB WinFont (txt$, x%, y%, Clr%, bold%)
DECLARE SUB SelectNewColor ()
DECLARE SUB HalfToneButton (x1%, y1%, w%, h%, F%, B%, L%, s%, S2%)
DECLARE SUB DrawIcon1 (Which%, x1%, y1%)
DECLARE FUNCTION GraphOptButton% (id%, x%, y%, w%, h%, icon%, xicon%, yicon%)
DECLARE SUB NewWin (x1%, y1%, w%, h%, B%, L%, s%, S2%, Caption$)
DECLARE SUB VarFulling ()
DECLARE SUB Tool.Curve ()
DECLARE SUB Tool.Polygon ()
DECLARE SUB Tool.Eraser ()
DECLARE SUB Tool.Dropper ()
DECLARE SUB Tool.Ellipse ()
DECLARE SUB Tool.Rectangle ()
DECLARE SUB Tool.AirBrush ()
DECLARE SUB Tool.Line ()
DECLARE SUB Tool.Fill ()
DECLARE SUB Tool.Pencil ()
DECLARE SUB AddItem2Menu (MenuID%, ItemID%, Caption$)
DECLARE SUB Save640 (FILE$)
DECLARE SUB DoMenuBar (ItemList$(), NoOfItems%, x%, y%)
DECLARE SUB MouseStatus (mb%, x%, y%)
DECLARE SUB QFont (txt$, x%, y%, C%, FontFile$, Attribs%)
DECLARE SUB Mouse (Action%)
DECLARE SUB FButon (x1%, y1%, w%, h%, bck%, light%, shadow%, shadow2%)
DECLARE SUB Item.InvertColrs ()
DECLARE SUB Item.Save ()
DECLARE SUB Item.ClearImg ()
DECLARE SUB Item.New ()
'$INCLUDE: 'qb.bi'
'****************************************************************************
COMMON SHARED PAX1%, PAY1%, PAX2%, PAY2% 'Paint Area Coords
COMMON SHARED BackClr%, ForeClr%, TheImgName$, FileModified%
COMMON SHARED TheFontPath$, StdFnt$, WinFnt$, SwapVideoFile$
COMMON SHARED UndoCount%, AcTool%
CLEAR , , 5000  '========================== Stack Space
CONST Show = 1, Hide = 2
CONST MAXOPT = 16
TYPE GraphicalOptionButton
	x AS INTEGER: y AS INTEGER: w AS INTEGER: h AS INTEGER
	icon AS INTEGER: xicon AS INTEGER: yicon AS INTEGER
	State AS STRING * 1
END TYPE
DIM SHARED MenuList$(8)
TheFontPath$ = "C:\QP4\FONTS\" '<=============== Change this to your own path
'TheFontPath$ = CurrDir$("C:") + "\" '<--- This line is for distribution only
StdFnt$ = TheFontPath$ + "verdana.fnt"
WinFnt$ = TheFontPath$ + "tahoma.fnt"
SwapVideoFile$ = "C:\SVF"
BMPName$ = "UNTITLED.BMP"
TheImgName$ = CurrDir$("C:") + "\" + BMPName$
BackClr% = 15: ForeClr% = 0
'************** TOOL.TEXT FONT CHANGE. IMPORTANT HOW TO... !? ***************
'****************************************************************************
' Someone of you have asked me how to change the Tool.Text Font.
' Here is the trick:
' Change the path in line #43 (Tool.Text SUB, TOOLS.BAS Module)
' This    >> ActualFont$ = ActualFont$ + "lucidabl.fnt"
' to this >> ActualFont$ = ActualFont$ + "MYFONT.fnt"
' MYFONT.fnt is a valid font file (use Font Ripper by Josh Heaton to create
' and add your own fonts) used by Quick Paint.
' In Tool.Text SUB are marked out some valid fonts that comes with the Quick
' Paint dist. pack (v.Alpha 4)
' TO DO: I'm actually working in a ComboBox that displays all the valid fonts
' to change between them (like the MS Paint does).
'****************************************************************************
DIM SHARED SelTool(16) 'Selected Tool
again:
 CLS
 SCREEN 12
'=== STARTING THE APPLICATION ============================================
 VarFulling
 NewWin 0, 0, 640, 480, 7, 15, 0, 8, "    Quick Paint ALPHA"
 DoMenuBar MenuList$(), 8, 15, 21
 RESTORE PaintIco
 x1% = 4: y1% = 3
 FOR y% = 1 TO 15
	 READ Info$
	FOR x% = 1 TO 15
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
 '=== THE PAINTING AREA ===================================================
 PAX1% = 60: PAY1% = 40: PAX2% = 60 + 560: PAY2% = 40 + 360
 FButon PAX1%, PAY1%, PAX2% - PAX1%, PAY2% - PAY1%, BackClr%, 0, 15, 8
 '=== THE SECTION OF COLORS ===============================================
	 C% = 0
	 FOR x% = 0 TO 7
			FButon 60 + (x% * 16), 410, 15, 15, C%, 0, 15, C%
			C2% = C% + 8
			FButon 60 + (x% * 16), 410 + 16, 15, 15, C2%, 0, 15, C2%
			C% = C% + 1
	 NEXT x%
	HalfToneButton 10, 410, 32, 32, 15, 7, 0, 15, 0
	FButon 23, 423, 15, 15, BackClr%, 15, 0, BackClr%
	FButon 13, 413, 15, 15, ForeClr%, 15, 0, ForeClr%
 '=== INFO BAR ============================================================
	FButon 10, 455, 620, 18, 7, 0, 15, 8
	WinFont "Welcome to Quick Paint ALPHA", 15, 460, 0, 1
 '=== TOOLS SETTINGS =====================================================
	FButon 5, 240, 50, 80, 7, 0, 15, 8
 '=== TITLE BAR ===========================================================
	FButon 150, 4, 380, 15, 1, 1, 1, 1
	QFont "- " + TheImgName$, 150, 3, 15, StdFnt$, 0
 '=========================================================================

 'LoadMouseCur 1
 Mouse Show
 
 DO
	
	 KEYB$ = INKEY$
	 MouseStatus mb%, mx%, my%
	 '------------------------------------------------------------------------
	 'Left-Side Tools
	 SelTool(1) = GraphOptButton(1, 6, 45, 24, 23, 1, 3, 3) 'Free Selection
	 SelTool(3) = GraphOptButton(2, 6, 69, 24, 23, 3, 5, 4) 'Ereaser
	 SelTool(5) = GraphOptButton(3, 6, 93, 24, 23, 5, 3, 3) 'Dropper
	 SelTool(7) = GraphOptButton(4, 6, 117, 24, 23, 7, 8, 3) 'Pencil
	 SelTool(9) = GraphOptButton(5, 6, 141, 24, 23, 9, 4, 4) 'Airbrush
	 SelTool(11) = GraphOptButton(6, 6, 165, 24, 23, 11, 5, 4) 'Line
	 SelTool(13) = GraphOptButton(7, 6, 189, 24, 23, 13, 4, 5) 'Rectangle
	 SelTool(15) = GraphOptButton(8, 6, 213, 24, 23, 15, 4, 5) 'Ellipse
	 'Right-Side Tools
	 SelTool(2) = GraphOptButton(9, 31, 45, 24, 23, 2, 4, 5) 'Selection
	 SelTool(4) = GraphOptButton(10, 31, 69, 24, 23, 4, 3, 4) 'Fill
	 SelTool(6) = GraphOptButton(11, 31, 93, 24, 23, 6, 4, 3)  'Zoom
	 SelTool(8) = GraphOptButton(12, 31, 117, 24, 23, 8, 7, 3) 'Brush
	 SelTool(10) = GraphOptButton(13, 31, 141, 24, 23, 10, 5, 4) 'Text
	 SelTool(12) = GraphOptButton(14, 31, 165, 24, 23, 12, 8, 4) 'Curve
	 SelTool(14) = GraphOptButton(15, 31, 189, 24, 23, 14, 4, 4) 'Polygon
	 SelTool(16) = GraphOptButton(16, 31, 213, 24, 23, 16, 4, 5) 'Rounded Box
	 '------------------------------------------------------------------------
	 Item% = MenuBar%(MenuList$(), 6, 15, 21, Menu%)
	 IF Menu% = 1 AND Item% = 1 THEN Item.New
	 IF Menu% = 1 AND Item% = 2 THEN Item.Open
	 IF Menu% = 1 AND Item% = 3 THEN Item.Save
	 IF Menu% = 1 AND Item% = 4 THEN Item.SaveAs
	 IF Menu% = 1 AND Item% = 6 THEN Sub2Quit
	 IF Menu% = 2 AND Item% = 1 THEN Item.Undo
	 IF Menu% = 4 AND Item% = 3 THEN Item.Settings
	 IF Menu% = 4 AND Item% = 4 THEN Item.ClearImg
	 IF Menu% = 5 AND Item% = 2 THEN Item.InvertColrs
	 '------------------------------------------------------------------------
	 IF (MSOverArea%(6, 45, 55, 235) = 1) AND mb% = 1 THEN
			FOR SelT% = 1 TO 16
				IF SelTool(SelT%) = 1 THEN
					InfoBar "Tools", SelT%
				END IF
			NEXT SelT%
			IF SelTool(4) = 1 THEN InfoBar "Tools", 4 'This is to fix a bug! To see it, just remed this line.
	 END IF
	 '------------------------------------------------------------------------
	 SelectNewColor
	 '------------------------------------------------------------------------
	 IF (MSOverArea%(PAX1%, PAY1%, PAX2%, PAY2%) = 1) AND mb% <> 0 THEN
			IF FileModified% <> 1 THEN FileModified% = 1
			IF SelTool(2) = 1 THEN GetUndoMem: Tool.Selection
			IF SelTool(3) = 1 THEN GetUndoMem: Tool.Eraser
			IF SelTool(4) = 1 THEN GetUndoMem: Tool.Fill
			IF SelTool(5) = 1 THEN GetUndoMem: Tool.Dropper
			IF SelTool(7) = 1 THEN GetUndoMem: Tool.Pencil
			IF SelTool(8) = 1 THEN GetUndoMem: Tool.Brush
			IF SelTool(9) = 1 THEN GetUndoMem: Tool.AirBrush
			IF SelTool(10) = 1 THEN GetUndoMem: Tool.Text
			IF SelTool(11) = 1 THEN GetUndoMem: Tool.Line
			IF SelTool(12) = 1 THEN GetUndoMem: Tool.Curve
			IF SelTool(13) = 1 THEN GetUndoMem: Tool.Rectangle
			IF SelTool(14) = 1 THEN GetUndoMem: Tool.Polygon
			IF SelTool(15) = 1 THEN GetUndoMem: Tool.Ellipse
			IF SelTool(16) = 1 THEN GetUndoMem: Tool.RoundedBox
	 END IF
	 '------------------------------------------------------------------------
 LOOP
 END
'***************************************************************************
PaintIco: '15x15
DATA "?@HHHHHHHHHHH@@","?@HOOH@GOOOOHH@","?@HOOGHGOOOOHOH"
DATA "H@HOOHHGOOHO@@@","HH@GOIAGOH@@GGG","?HG@GIAGCK@GOGG"
DATA "?@DL@EACK@GGOOG","?@HDDACK@GGOOOG","HHHHHHHHH@GOOOG"
DATA "HGOOOOGGHHGOOOG","?HGGHHHH@GGOOOG","?HOOGGHHHGGOOOG"
DATA "?HOOOGHH@GGOOOG","?HOOOGGHHGGOOOG","?HOOOGGH@GGGGGG"
FreeSel: '16x15
DATA "????@@??????@???","????@??????@@???","???????@??@?????"
DATA "????@???@???@???","????@???????@???","????????????????"
DATA "???@????????@???","??@??????????@??","?@????????????@?"
DATA "@@?@@????@@?@@@@","????@???@???????","????????????????"
DATA "????@?@?????????","????@@??????????","????@???????????"
Selection: '15x11
DATA "@?@@?@@?@@?@@?@","@?????????????@","???????????????"
DATA "@?????????????@","@?????????????@","???????????????"
DATA "@?????????????@","@?????????????@","???????????????"
DATA "@?????????????@","@?@@?@@?@@?@@?@"
Rubber: '14x11
DATA "???????@@@@@@?","??????@NONONO@","?????@NONONO@@"
DATA "????@NONONO@G@","???@NONONO@GN@","??@NONONO@GN@?"
DATA "?@NONONO@GN@??","@GGGGGG@GN@???","@GGGGGG@N@????"
DATA "@GGGGGG@@?????","?@@@@@@@??????"
Filler: '16x13
DATA "????????@@??????","???????@O@??????","??@@@?@OOO@@????"
DATA "?@@@?@OOO@OO@???","@@@?@CCCCCC@@@??","@@?@CCCCCC@@@@@?"
DATA "@@?@@C@CC@@@@@C@","@@@??@CC@@@@@CC@","@@@??@C@@@@@CC@?"
DATA "@@@???@@@@@CC@??","@@@????@@@CC@???","?@@?????@CC@????"
DATA "?@@??????@@?????"
Droper: '16x15
DATA "????????????@@@?","???????????@O@@@","??????????@@@@@@"
DATA "????????@@@@@@@@","?????????@@@@@@?","????????@O@@@@??"
DATA "???????@OOH@@???","??????@OOH@?@???","?????@OOH@??????"
DATA "????@OOC@???????","???@OOC@????????","??@OOC@?????????"
DATA "?C@CC@CCC???????","CC@@@CCCCC??????","?CCCCCCC????????"
Zoomer: '15x16
DATA "????@@@@???????","??@@GGGG@@?????","?@HOOOGGGH@????"
DATA "?@OOGGGGGG@????","@GOGGGGGGGG@???","@GOGGGGGGGG@???","@GGGGGGGGGG@???"
DATA "@GGGGGGGGGG@???","?@GGGGGGGO@????","?@HGGGGGOH@????","??@@GGGOHAAA???"
DATA "????@@@@AGAAA??","?????????AGAAA?","??????????AGAAA"
DATA "???????????AGAA","????????????AA?"
Pencil: '9x15
DATA "?????@@@?","????@DDD@","????@DOO@","???@@DO@?"
DATA "???@N@@@?","??@NOG@??","??@ON@@??","?@ONG@???"
DATA "?@NO@@???","@NOG@????","@@N@@????","@@@@?????"
DATA "@@@??????","@@???????","@????????"
Brush:  '8x16
DATA "???@@???","??@FF@??","??@FF@??","??@FF@??"
DATA "??@FF@??","??@FF@??","??@FF@??","?@FFFF@?"
DATA "@FFFFFF@","@@@@@@@@","@@@@@@@@","@OOOOOO@","@OHOHOO@"
DATA "@OHOHOO@","@OHOHOO@","@@@@@@@@"
Airbrush: '16x13
DATA "??A?A??A????????","?AGAGAAA@@@?????","AGAGA?@@OO@@????"
DATA "?AGA??@OO@@II???","AGA???@O@@III@??","?AG???@@AIAIII@?"
DATA "AGA????@AAIAIII@","?A??????@AAIAII@","AGA??????@AAIA@?"
DATA "?A????????@AA@??","A??????????@@???","?A??????????????"
DATA "A???????????????"
DrawText:  '14x12
DATA "??????@???????","?????@@@??????","?????@@@??????"
DATA "?????@@@@?????","????@?@@@?????","????@?@@@@????"
DATA "???@???@@@????","???@???@@@@???","???@@@@@@@@???"
DATA "??@?????@@@@??","??@??????@@@??","@@@@???@@@@@@@"
DLine: '14x13
DATA "@@????????????","?@@???????????","??@@??????????"
DATA "???@@?????????","????@@????????","?????@@???????"
DATA "??????@@??????","???????@@?????","????????@@????"
DATA "?????????@@???","??????????@@??","???????????@@?"
DATA "????????????@@"
Curve: '6x14
DATA "???@@?","????@@","?????@","?????@","????@@","???@@?"
DATA "??@@??","?@@???","@@????","@?????","@?????"
DATA "@@????","?@@???","??@@??"
Rectangl: '15x11
DATA "@@@@@@@@@@@@@@@","@?????????????@","@?????????????@"
DATA "@?????????????@","@?????????????@","@?????????????@"
DATA "@?????????????@","@?????????????@","@?????????????@"
DATA "@?????????????@","@@@@@@@@@@@@@@@"
Polygon: '14x13
DATA "????@@@@@@@@@?","????@??????@??","????@??????@??"
DATA "???@??????@???","???@??????@???","??@??????@????"
DATA "??@??????@????","?@??????@?????","?@?????@@@@@@@"
DATA "?@???????????@","@????????????@","@????????????@"
DATA "@@@@@@@@@@@@@@"
Ellipse: '15x10
DATA "????@@@@@@@????","??@@???????@@??","?@???????????@?"
DATA "@?????????????@","@?????????????@","@?????????????@"
DATA "@?????????????@","?@???????????@?","??@@???????@@??"
DATA "????@@@@@@@????"
RoundedBox: '15x11
DATA "??@@@@@@@@@@@??","?@???????????@?","@?????????????@"
DATA "@?????????????@","@?????????????@","@?????????????@"
DATA "@?????????????@","@?????????????@","@?????????????@"
DATA "?@???????????@?","??@@@@@@@@@@@??"

SUB DrawIcon1 (Which%, x1%, y1%)
SELECT CASE Which%
CASE 1
RESTORE FreeSel
 FOR y% = 1 TO 15
	 READ Info$
	FOR x% = 1 TO 16
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 2
RESTORE Selection
 FOR y% = 1 TO 11
	 READ Info$
	FOR x% = 1 TO 15
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 3
RESTORE Rubber
 FOR y% = 1 TO 11
	 READ Info$
	FOR x% = 1 TO 14
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 4
RESTORE Filler
 FOR y% = 1 TO 13
	 READ Info$
	FOR x% = 1 TO 16
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 5
RESTORE Droper
 FOR y% = 1 TO 15
	 READ Info$
	FOR x% = 1 TO 16
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 6
RESTORE Zoomer
 FOR y% = 1 TO 16
	 READ Info$
	FOR x% = 1 TO 15
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 7
RESTORE Pencil
 FOR y% = 1 TO 15
	 READ Info$
	FOR x% = 1 TO 9
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 8
RESTORE Brush
 FOR y% = 1 TO 16
	 READ Info$
	FOR x% = 1 TO 8
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 9
RESTORE Airbrush
 FOR y% = 1 TO 13
	 READ Info$
	FOR x% = 1 TO 16
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 10
RESTORE DrawText
 FOR y% = 1 TO 12
	 READ Info$
	FOR x% = 1 TO 14
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 11
RESTORE DLine
 FOR y% = 1 TO 13
	 READ Info$
	FOR x% = 1 TO 14
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 12
RESTORE Curve
 FOR y% = 1 TO 14
	 READ Info$
	FOR x% = 1 TO 6
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 13
RESTORE Rectangl
 FOR y% = 1 TO 11
	 READ Info$
	FOR x% = 1 TO 15
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 14
RESTORE Polygon
 FOR y% = 1 TO 13
	 READ Info$
	FOR x% = 1 TO 14
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 15
RESTORE Ellipse
 FOR y% = 1 TO 10
	 READ Info$
	FOR x% = 1 TO 15
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
CASE 16
RESTORE RoundedBox
 FOR y% = 1 TO 11
	 READ Info$
	FOR x% = 1 TO 15
	 TheClr$ = MID$(Info$, x%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (x% + x1%, y% + y1%), C%
 NEXT x%, y%
END SELECT
END SUB

SUB GetUndoMem
 UndoCount% = 1
 Mouse Hide
	Save640 "\undomem"
 Mouse Show
END SUB

FUNCTION GraphOptButton% (id%, x%, y%, w%, h%, icon%, xicon%, yicon%)
STATIC Counter, Redimed, OldOptBIndex, ButtonSelected
STATIC OptB() AS GraphicalOptionButton
IF Redimed = 0 THEN
 REDIM OptB(MAXOPT) AS GraphicalOptionButton
 Redimed = 1
END IF
IF (CheckBit%(OptB(id%).State, 4) = 0) THEN 'If Option Button is not drawn...
	 Mouse Hide                               'just draw it
	 FButon x%, y%, w%, h%, 7, 15, 0, 7
	 DrawIcon1 icon%, x% + xicon%, y% + yicon%
	 Mouse Show
	 OptB(id%).x = x%: OptB(id%).y = y%: OptB(id%).w = w%: OptB(id%).h = h%
	 OptB(id%).icon = icon%: OptB(id%).xicon% = xicon%: OptB(id%).yicon = yicon%
	 OptB(id%).State = ChangeBit$(OptB(id%).State, 3, 0) 'Selected  = FALSE
	 OptB(id%).State = ChangeBit$(OptB(id%).State, 4, 1) 'Drawn = TRUE
END IF
 MouseStatus mb%, mx%, my%
 IF (MSOverArea%(x%, y%, x% + w%, y% + h%) = 1) AND mb% = 1 THEN
	 Mouse Hide
	 IF (CheckBit%(OptB(OldOptBIndex).State, 4) = 1) THEN
		 OI = OldOptBIndex
		 FButon OptB(OI).x, OptB(OI).y, OptB(OI).w, OptB(OI).h, 7, 15, 0, 7
		 DrawIcon1 OptB(OI).icon, OptB(OI).x + OptB(OI).xicon, OptB(OI).y + OptB(OI).yicon
		 OptB(OI).State = ChangeBit$(OptB(OI).State, 3, 0) 'Old Selected = FALSE
	 END IF
	 'FButon x%, y%, w%, h%, 7, 0, 15, 7
	 HalfToneButton x%, y%, w%, h, 15, 7, 0, 15, 7
	 DrawIcon1 icon%, x% + xicon% + 1, y% + yicon% + 1
	 OptB(id%).State = ChangeBit$(OptB(id%).State, 3, 1) 'New Selected = TRUE
	 Mouse Show
	 DO
		 MouseStatus mb%, mx%, my%
	 LOOP UNTIL mb% = 0
 'GraphOptButton% = CheckBit%(OptB(ID%).State, 3) 'This give a TRUE state in
 'one loop, and in the rest of the loops it becomes FALSE!
 OldOptBIndex = id%
 END IF
 GraphOptButton% = CheckBit%(OptB(id%).State, 3) 'This gives a TRUE state in
 'every loop it is called, ie, gives TRUE always...
END FUNCTION

SUB HalfToneButton (x1%, y1%, w%, h%, F%, B%, L%, s%, S2%)
 LINE (x1%, y1%)-(x1% + w%, y1% + h%), B%, BF
 FOR y% = y1% TO y1% + h% STEP 2
	 FOR x% = x1% TO x1% + w% STEP 2
		PSET (x%, y%), F%
	 NEXT x%
 NEXT y%
 LINE (x1%, y1%)-(x1% + w%, y1% + h%), L%, B
 LINE (x1% + w%, y1% + h%)-(x1% + w%, y1%), s%, B
 LINE (x1% + w%, y1% + h%)-(x1%, y1% + h%), s%, B
' LINE (x1% + w% - 1, y1% + h% - 1)-(x1% + w% - 1, y1% + 1), S2%, B
' LINE (x1% + w% - 1, y1% + h% - 1)-(x1% + 1, y1% + h% - 1), S2%, B
END SUB

SUB InfoBar (InfoFor$, InfoID%)
AcTool% = InfoID%
ToolsSettingsDraw InfoID%
SELECT CASE InfoFor$
CASE "Tools"
	 SELECT CASE InfoID%
	 CASE 1
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Selects an area in free form mode to move, copy or modify", 20, 459, 0, 1
	 CASE 2
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Selects an image rectangular area to move, copy or modify", 20, 459, 0, 1
	 CASE 3
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Ereases and image sector using the selected erease form", 20, 459, 0, 1
	 CASE 4
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Fills and area with the actual draw color taking background color as fill limit", 20, 459, 0, 1
	 CASE 5
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Selects an image color to draw", 20, 459, 0, 1
	 CASE 6
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Changes the magnify", 20, 459, 0, 1
	 CASE 7
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Draws a free form line of one pixel width", 20, 459, 0, 1
	 CASE 8
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Draws using a brush from selected form and size ", 20, 459, 0, 1
	 CASE 9
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Draws using a selected sized airbrush", 20, 459, 0, 1
	 CASE 10
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Inserts text inside the image", 20, 459, 0, 1
	 CASE 11
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Draws a rect line with the selected width", 20, 459, 0, 1
	 CASE 12
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Draws a curve line with the selected width", 20, 459, 0, 1
	 CASE 13
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Draws a rectangle with the selected fill style", 20, 459, 0, 1
	 CASE 14
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Draws a polygon with the selected fill style", 20, 459, 0, 1
	 CASE 15
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Draws an ellipse with the selected fill style", 20, 459, 0, 1
	 CASE 16
		FButon 10, 455, 620, 18, 7, 0, 15, 8
		WinFont "Draws a corner-rounded rectangle with the selected fill style", 20, 459, 0, 1
	 END SELECT
END SELECT
END SUB

SUB LoadBmp (Name$, x1%, y1%)
DIM id AS STRING * 2, size AS LONG, rr1 AS INTEGER, rr2 AS INTEGER
DIM offset AS LONG, horz AS LONG, wid AS LONG, hei AS LONG
DIM planes AS INTEGER, bpp AS INTEGER, pakbyte AS LONG, imagebytes AS LONG
DIM xres AS LONG, yres AS LONG, colch AS LONG, ic AS LONG
DIM pal AS STRING * 1024

OPEN Name$ FOR BINARY AS #1
GET #1, , id: GET #1, , size: GET #1, , rr1: GET #1, , rr2: GET #1, , offset
GET #1, , horz: GET #1, , wid: GET #1, , hei: GET #1, , planes: GET #1, , bpp
GET #1, , pakbyte: GET #1, , imagebytes: GET #1, , xres: GET #1, , yres
GET #1, , colch: GET #1, , ic: GET #1, , pal

a$ = pal                  ' Pal is stored in a 1024 character string
OUT &H3C8, 0              ' Start writing from Colour 0
St% = 4
FOR I% = 1 TO 1024 STEP St%
				B% = ASC(MID$(a$, I%, 1)) \ 4      'blue
				g% = ASC(MID$(a$, I% + 1, 1)) \ 4  'green
				r% = ASC(MID$(a$, I% + 2, 1)) \ 4  'red ' I% + 3 is set to zero.
				OUT &H3C9, r%           ' Set the colour.
				OUT &H3C9, g%
				OUT &H3C9, B%
NEXT

Pixel$ = SPACE$(wid + 2)
iHeight% = hei - 1
iWidth% = wid - 3
FOR y% = iHeight% TO 0 STEP -1
	GET #1, , Pixel$
	FOR x% = 0 TO iWidth% + 2
		PSET (x1% + x%, y1% + y%), ASC(MID$(Pixel$, x% + 1, 1))
	NEXT x%
NEXT y%
CLOSE #1

END SUB

SUB Millidelay (msecs) STATIC
IF sysfact& THEN                   'Did we calc- system speed yet?
				IF msecs THEN                  'have to want a delay
						count& = (sysfact& * msecs) \ -54    'calc- # of loops needed
						DO
								count& = count& + 1         'negative - add to get to 0
								IF count& = z THEN EXIT DO  'when its 0 we're done
						LOOP UNTIL t2 = PEEK(&H6C)      'make it the same as loop below
				END IF
		ELSE                               'if sysfact& is 0, calc- system speed
				DEF SEG = &H40                 'point to low memory
				t1 = PEEK(&H6C)                'get tick count
				DO
						t2 = PEEK(&H6C)            'get tick count
				LOOP UNTIL t2 <> t1            'wait 'til its a new tick
				DO
						sysfact& = sysfact& + 1         'count number of loops
						IF sysfact& = z THEN EXIT DO    'make it the same as loop above
				LOOP UNTIL t2 <> PEEK(&H6C)         'keep going 'til its a new tick
				t2 = 256                            'prevent the above UNTIL
		END IF
END SUB

DEFSNG A-Z
SUB NewWin (x1%, y1%, w%, h%, B%, L%, s%, S2%, Caption$)
 WCB = B%: WCL = L%: WCS = s%: WCS2 = S2%
 CTB% = 1
 WorkArea% = bck%: BarSep% = 19
 LINE (x%, y%)-(x% + w%, y%), WCB
 LINE (x%, y% + 1)-(x% + w%, y% + 1), WCL
 LINE (x%, y% + 2)-(x% + w%, y% + 3), WCB, B
 LINE (x%, y%)-(x%, y% + h%), WCB
 LINE (x% + 1, y% + 1)-(x% + 1, y% + h%), WCL
 LINE (x% + 2, y% + 2)-(x% + 3, y% + h%), WCB, B
 LINE (x%, y% + h%)-(x% + w%, y% + h%), WCS
 LINE (x% + 1, y% + h% - 1)-(x% + w%, y% + h% - 1), WCS2
 LINE (x% + 2, y% + h% - 2)-(x% + w%, y% + h% - 3), WCB, B
 LINE (x% + w%, y%)-(x% + w%, y% + h%), WCS
 LINE (x% + w% - 1, y% + 1)-(x% + w% - 1, y% + h% - 1), WCS2
 LINE (x% + w% - 2, y% + 2)-(x% + w% - 3, y% + h% - 2), WCB, B
 '******* Work Area
	LINE (x% + 4, y% + 4)-(x% + w% - 4, y% + h% - 4), WCB, BF
 '*******
 LINE (x% + 4, y% + 4)-(x% + w% - 4, y% + BarSep%), CTB%, BF
 LINE (x% + 4, y% + 4)-(x% + w% - 4, y% + BarSep%), CTB%, B
 LINE (x% + 4, y% + BarSep%)-(x% + w% - 4, y% + BarSep%), CTB%
 LINE (x% + w% - 4, y% + BarSep%)-(x% + w% - 4, y% + 4), CTB%
 QFont Caption$, x% + 6, y% + 3, 15, StdFnt$, 1
END SUB

DEFINT A-Z
SUB PutUndoMem
IF UndoCount% = 1 THEN
 Mouse Hide
	Load640 "\undomem"
 Mouse Show
		 UndoMemFile$ = "\undomem"
		 KILL UndoMemFile$ + ".blu"
		 KILL UndoMemFile$ + ".grn"
		 KILL UndoMemFile$ + ".red"
		 KILL UndoMemFile$ + ".int"
 UndoCount% = 0
END IF
END SUB

SUB SelectNewColor
MouseStatus mb%, mx%, my%
IF (MSOverArea%(60, 410, 187, 442) = 1) AND mb% <> 0 THEN
	 FOR x% = 0 TO 7
			IF MSOverArea%(60 + (x% * 16), 410, 60 + (x% * 16) + 15, 410 + 15) = 1 THEN
				 MouseStatus mb%, mx%, my%
				 IF mb% = 1 THEN ForeClr% = x%
				 IF mb% = 2 THEN BackClr% = x%
			END IF
			IF MSOverArea%(60 + (x% * 16), 410 + 16, 60 + (x% * 16) + 15, 410 + 31) = 1 THEN
				 MouseStatus mb%, mx%, my%
				 IF mb% = 1 THEN ForeClr% = x% + 8
				 IF mb% = 2 THEN BackClr% = x% + 8
			END IF
	 NEXT x%
		 Mouse Hide
		 'HalfToneButton 10, 410, 32, 32, 15, 7, 0, 15, 0
		 FButon 23, 423, 15, 15, BackClr%, 15, 0, BackClr%
		 FButon 13, 413, 15, 15, ForeClr%, 15, 0, ForeClr%
		 Mouse Show
		 DO
			 MouseStatus mb%, mx%, my%
		 LOOP UNTIL mb% = 0
END IF
END SUB

SUB Sub2Quit
 IF UndoCount% = 1 THEN
		 UndoMemFile$ = "\undomem"
		 KILL UndoMemFile$ + ".blu"
		 KILL UndoMemFile$ + ".grn"
		 KILL UndoMemFile$ + ".red"
		 KILL UndoMemFile$ + ".int"
 END IF
 END
END SUB

SUB VarFulling
 MenuList$(1) = "File"
 MenuList$(2) = "Edit"
 MenuList$(3) = "View"
 MenuList$(4) = "Image"
 MenuList$(5) = "Colors"
 MenuList$(6) = "Help"

 AddItem2Menu 1, 1, "New"
 AddItem2Menu 1, 2, "Open"
 AddItem2Menu 1, 3, "Save"
 AddItem2Menu 1, 4, "Save as"
 AddItem2Menu 1, 5, "Print"
 AddItem2Menu 1, 6, "Exit"

 AddItem2Menu 2, 1, "Undo"
 AddItem2Menu 2, 2, "Cut"
 AddItem2Menu 2, 3, "Copy"
 AddItem2Menu 2, 4, "Paste"

 AddItem2Menu 3, 1, "Zoom"

 AddItem2Menu 4, 1, "Rotate"
 AddItem2Menu 4, 2, "Expand"
 AddItem2Menu 4, 3, "Settings"
 AddItem2Menu 4, 4, "Clear image"

 AddItem2Menu 5, 1, "Modify colors"
 AddItem2Menu 5, 2, "Invert colors"

 AddItem2Menu 6, 1, "Help Topics"
 AddItem2Menu 6, 2, "About..."

END SUB

SUB WinFont (txt$, x%, y%, Clr%, bold%)
real.bold% = bold%
WHILE bold% > -1
DRAW "BM " + LTRIM$(STR$(x%)) + "," + LTRIM$(STR$(y%)) + " C" + LTRIM$(STR$(Clr%))
FOR k = 1 TO LEN(txt$)
Char$ = MID$(txt$, k, 1)
SELECT CASE Char$
CASE CHR$(251)
DRAW "BD5 F2 E1U1E1U1E1U1E1 BR2"
CASE "@"
DRAW "BD3 D3 F1 D1 R1 F1 R4 BU3 L1 R2 U3 H1 U1 L1 H1 L3 G1 L1 D1 G1 BR4 R2 D2 G1 L1 H1 U1 BU4 BR8"
CASE " "
DRAW "BR4"
CASE ":"
DRAW "BR1 BD3 D1 BD4 U1 BU7 BR3"
CASE ";"
DRAW "BR1 BD3 D1 BD4 D1 G1 E1 U1 U1 BU7 BR3"
CASE ","
DRAW "BR1 BD3 BD1 BD4 D1 G1 E1 U1 U1 BU7 BR3"
CASE "\"
DRAW "D1 F1 D1 F1 D1 F1 D1 F1 BU8 BR2"
CASE "/"
DRAW "BD8 E1 U1 E1 U1 E1 U1 E1 U1 BR2"
CASE ">"
DRAW "F4 G4 BU8 BR6"
CASE "<"
DRAW "BR4 G4 F4 BU8 BR2"
CASE "="
DRAW "BD3 R3 BD3 L3 R3 BU6 BR2"
CASE "-"
DRAW "BD5 R4 BU5 BR2"
CASE "+"
DRAW "BR3 BD1 D7 U4 L3 R6 BU4 BR2"
CASE "."
DRAW "BR1 BD8 U1 BU7 BR3"
CASE "="
DRAW "BD1 R3 BD3 L3 R3 BU7 BR2"
CASE "!"
DRAW "BR1 D5 BD2 D1 BU8 BR2"
CASE "A"
DRAW "BD7 D1 U1 E1 R4 L4 U2 E1 U1 E1 U1 D1 F1 D1 F1 D2 F1 D1 BU8 BR2"
CASE "B"
DRAW "D8 U8 R3 F1 D2 G1 L2 R2 F1 D2 G1 L3 R3 BU8 BR3"
CASE "C"
DRAW "BD1 D6 F1 R3 E1 BU6 H1 L3 R3 BR3"
CASE "D"
DRAW "D8 R4 E1 U6 H1 L4 R4 BR3"
CASE "E"
DRAW "D8 R3 L3 U4 R2 L2 U4 R3 BR2"
CASE "F"
DRAW "D8 U4 R2 L2 U4 R3 BR2"
CASE "G"
DRAW "BD1 D6 F1 R2 E1 R1 D1 U4 L3 R3 BU3 H1 L3 R3 BR3"
CASE "H"
DRAW "D8 U4 R4 D4 U8 BR2"
CASE "I"
DRAW "R2 L1 D8 L1 R2 BU8 BR2"
CASE "J"
DRAW "BD7 F1 R1 E1 U7 BR2"
CASE "K"
DRAW "D8 U8 D4 R1 E3 U1 BD8 U1 H3 L1 BU4 BR5"
CASE "L"
DRAW "D8 R3 BU8 BR2"
CASE "M"
DRAW "D8 U6 R1 D1 F1 D1 F1 D1 U1 E1 U1 E1 U1 R1 U2 D8 U8 BR2"
CASE "N"
DRAW "D8U8 F1D1 F1D1 F1D1 F1R1 D1 U8 BR2"
CASE "O" 'letter
DRAW "BD1 D6 F1 R3 E1 U6 H1 L3 R3 BR3"
CASE "P"
DRAW "D8 U4 R3 E1 U2 H1 L3 R3 BR3"
CASE "Q"
DRAW "BD1 D6 F1 R3 U1 H1 F1 D1 F1 BU1 U7 H1 L3 R3 BR3"
CASE "R"
DRAW "D8 U8 R3 F1 D2 G1 L2 R2 F1 D3 R1 BU8 BR2"
CASE "S"
DRAW "BD1 D2 F1 R2 F1 D2 G1 L2 H1 BU7 BR1 R2 F1 BU1 BR2"
CASE "T"
DRAW "R4 L2 D8 U8 BR4"
CASE "U" 'you
DRAW "D7 F1 R3 E1 U7 BR2"
CASE "V" 'vee
DRAW "D1 F1 D2 F1 D1 F1 D1 U1 E1 U1 E1 U2 E1 U1 BR2"
CASE "W"
DRAW "D1 F1 D2 F1 D1 F1 D1 U1 E1 U1 E1 U2 D2 F1 D1 F1 D1 U1 E1 U1 E1 U2 E1 U1 BR2"
CASE "X"
DRAW "D2 F1 D2 G1 D2 BR4 U2 H1 U2 E1 U2 BR2"
CASE "Y"
DRAW "F3 D5 U5 E3 BR2"
CASE "Z"
DRAW "BR1 R5 D1 R0 G6 D1 R5 BU8 BR2"
CASE "a"
DRAW "BD6 D1 F1 R3 U3 L3 R3 U1 H1 L2 BR3 BU3 BR2"
CASE "b"
DRAW "D8 R3 E1 U3 H1 L3 R3 BR1 BU3 BR2"
CASE "c"
DRAW "BD4 D3 F1 R2 E1 BU3 H1 L2 R2 BR3 BU3"
CASE "d"
DRAW "BD4 D3 F1 R3 U5 L3 R3 U3 BR2"
CASE "e"
DRAW "BD4 D3 F1 R2 E1 BU2 L3 R3 U1 H1 L2 R2 BR3 BU3"
CASE "f"
DRAW "BR1 G1 D7 U4 R1 BU4 BR2"
CASE "g"
DRAW "BD5 D2 F1 R2 E1 D4 L3 R3 U6 H1 L2 R2 BR1 BU4 BR2"
CASE "h"
DRAW "D8 U4 R1 E1 R1 F1 D4 BU8 BR2"
CASE "i"
DRAW "BR1 D1 BD2 D5 U5 BU3 BR3"
CASE "j"
DRAW "BD9 F1 R1 E1 U5 BU2 U1 BU1 BR2"
CASE "k"
DRAW "D8 U3 R1 E2 G2 F3 BU8 BR2"
CASE "l"  'small L'
DRAW "BR1 D7 F1 H1 U7 BR3"
CASE "m"
DRAW "BD3 D5 U5 R2 F1 D3 U3 E1 R1 F1 D4 BU8 BR2"
CASE "n"
DRAW "BD3 D5 U4 R1 E1 R1 F1 D4 BU8 BR2"
CASE "o"
DRAW "BD4 D3 F1 R2 E1 U3 H1 L2 R2 BR3 BU3"
CASE "p"
DRAW "BD3 D7 U7 R2 F1 D2 G1 L2 R2 E1 BU6 BR2"
CASE "q"
DRAW "BD5 D2 F1 R2 E1 D4 R2 L2 U6 H1 L2 R2 BR1 BU4 BR2"
CASE "r"
DRAW "BD3 R1 L1 D5 U5 BU3 BR3"
CASE "s"
DRAW "BD4 F3 G1 L1 H1 BU4 BR1 R1 F1 BU4 BR2"
CASE "t"
DRAW "BD1 D6 F1 H1 U4 R1 BU3 BR2"
CASE "u"
DRAW "BD3 D4 F1 R2 E1 U4D4 F1 BU8 BR2"
CASE "v"
DRAW "BD3 D2 F2 D1U1 E2 U2 BU3 BR2"
CASE "w"
DRAW "BD3 D3 F1 D1 U1 E1 U1 E1 U1 D1 F1 D1 F1 D1 U1 E1 U3 BU3 BR2"
CASE "x"
DRAW "BD3 D1 F1 D1 G1 D1 BR3 U1 H1 U1 E1 U1 BU3 BR2"
CASE "y"
DRAW "BD3 D4 F1 R2 E1 D4 L2 R2 U8 BU3 BR2"
CASE "z"
DRAW "BD3 R3 D1 G3 D1 R3 BU8 BR2"
CASE "0" 'number
DRAW "BD1 D6 F1 R2 E1 U6 H1 L2 R2 BR3"
CASE "1" 'number
DRAW "BR1 D8 U8 G1 E1 BR3"
CASE "2"
DRAW "BD1 E1 R2 F1 D2 G4 D1 R4 BU8 BR2"
CASE "3"
DRAW "BD1 E1 R2 F1 D2 G1 L1 R1 F1 D2 G1 L2 H1 BU7 BR6"
CASE "4"
DRAW "BR3 D8 U2 R1 L4 U1 E1 U1 E1 U1 E1 BR2"
CASE "5"
DRAW "R3 L3 D3 R2 F1 D3 G1 L2 H1 BU7 BR6"
CASE "6"
DRAW "BD1 D6 F1 R2 E1 U2 H1 L3 U3 E1 R2 F1 BU1 BR2"
CASE "7"
DRAW "R4 D1 G1 D1 G1 D1 G1 D2 BU8 BR5"
CASE "8"
DRAW "BD1 D2 F1 G1 D2 F1 R3 E1 U2 H1 L2 R2 E1 U2 H1 L3 R3 BR3"
CASE "9"
DRAW "BD1 D2 F1 R3 E1 D4 G1 L3 H1 BU7 BR1 R3 F1 D6 U6 BU1 BR2"
END SELECT
IF real.bold% > 0 THEN DRAW "BR1"
NEXT k
bold% = bold% - 1
x% = x% + 1
WEND
END SUB


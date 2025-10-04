'** If you are reading this message you loaded the wrong module
'** It doesn't do anything by itself.
'** PLEASE, In order to run the full application you need to
'** to load QUICKPT4.BAS....

DEFINT A-Z
DECLARE FUNCTION CurrDir$ (Dr$)
DECLARE FUNCTION OpenFileDialog$ (Drive$, Path$, WhichFileType$)

DECLARE SUB IniScreen ()
DECLARE SUB CheckForIcons (TheFile$, xd%, yd%)
DECLARE SUB DrIcon (Which%, xr%, yr%)
DECLARE SUB ArrowUp (X1%, Y1%, wdth%, height%, status%, light&, shadow&)
DECLARE SUB ArrowDown (X1%, Y1%, wdth%, height%, status%, light&, shadow&)
DECLARE SUB ArrowLeft (X1%, Y1%, wdth%, height%, status%, light&, shadow&)
DECLARE SUB ArrowRight (X1%, Y1%, wdth%, height%, status%, light&, shadow&)
DECLARE SUB QFont (txt$, X%, Y%, C%, FontFile$, Attribs%)
DECLARE SUB Button2 (X1%, Y1%, wdth%, height%, bck%, light%, shadow%, shadow2%)
DECLARE SUB Button (X1%, Y1%, wdth%, height%, bck%, light%, shadow%)
'OPEN DIALOG BY JG
'****************************************************************************
DECLARE SUB WriteFileInfo ()
DECLARE FUNCTION DirX% (Dirspec$, Filter%, SearchType%)
'$INCLUDE: 'qb.bi'
'$DYNAMIC
'****************************************************************************
TYPE FontCharInfo: CharWidth AS INTEGER: CharHeight AS INTEGER: FileOffset AS LONG: END TYPE
CONST ALLENTRIES = 0, FILESONLY = 1, DIRSONLY = 2
CONST FALSE = 0, TRUE = NOT FALSE
CONST FINDFIRST = &H4E00, FINDNEXT = &H4F00
CONST NOTMOREFOUND = 0, ENTRYNOTMATCHEDFILTER = 1, ENTRYFOUND = 2
'CONST Show = 1, Hide = 2

CONST Show = 1, Hide = 2 'Constants for Mouse SUB
COMMON SHARED PAX1%, PAY1%, PAX2%, PAY2% 'Paint Area Coords
COMMON SHARED BackClr%, ForeClr%, TheImgName$, FileModified%
COMMON SHARED TheFontPath$, StdFnt$, WinFnt$, SwapVideoFile$


TYPE DTAStructure
	 DOS AS STRING * 19: CreateTime AS STRING * 1
	 Attributes AS INTEGER: AccessTime AS INTEGER: AccessDate AS INTEGER
	 FileSize AS LONG: FileName AS STRING * 13
END TYPE
DIM SHARED Regs AS RegTypeX
DIM SHARED DTA AS DTAStructure
DIM SHARED CurrentDir$, FileType$, FileName$
DIM SHARED HMDir%, HMFile%', WinFnt$
DIM SHARED ViewedFiles$(60), XK%, YK%, FTV AS INTEGER
DIM SHARED DirsANDFiles$(1700)
HMDir% = 0: HMFile% = 0
FTV = 15 'Files To View
'WinFnt$ = "c:\hires\bas\fonts\fnt\tahoma.fnt"




SCREEN 12
CLS
ReOpen:
Button 0, 0, 640, 480, 3, 3, 3
IniScreen
FileSelected$ = OpenFileDialog$("C:", CurrDir$("C:"), "*.*")
IF FileSelected$ <> "ERROR" THEN PRINT "You Selected:"; FileSelected$

Folder: '15x13
DATA  "??HHHHH????????","?HNNNNNH???????","HHHHHHHHHHHHHH?","HNNNNNNNNNNNNG@"
DATA  "HNNNNNNNNNNNNG@","HNNNNNNNNNNNNG@","HNNNNNNNNNNNNG@","HNNNNNNNNNNNNG@"
DATA  "HNNNNNNNNNNNNG@","HNNNNNNNNNNNNG@","HNNNNNNNNNNNNG@","HGGGGGGGGGGGGG@"
DATA  "?@@@@@@@@@@@@@@"
BMP: '15x16
DATA "????@@@?@@@????","@@??@L@?@K@??@?","@I@?@@@?@@@?@@@","@AI@@L@?@K@@NF@"
DATA "?@AI@L@?@K@NF@?","??@AI@@?@@NF@??","???@AI@?@NF@???","?@@@@@@@@@@@@??"
DATA "?@OOOOGGGGGH@??","??@@@@@@@@@@???","??@OOOGGHHH@???","??@OOOGGHHH@???"
DATA "??@OOOGGGGH@???","??@OOOOOGGH@???","??@OOOOOGGH@???","??@@@@@@@@@@???"
UnknownFile: '13x15
DATA "HHHHHHHHHH???","HOOOOOOOOGH??","HOOOOOOOOGOH?","HOOOOOOOO@@@@"
DATA "HOOO@@@@OOOG@","HOO@@OO@@OOG@","HOO@@OO@@OOG@","HOOOOO@@OOOG@"
DATA "HOOOO@@OOOOG@","HOOOOOOOOOOG@","HOOOO@@OOOOG@","HOOOO@@OOOOG@"
DATA "HOOOOOOOOOOG@","HGGGGGGGGGGG@","@@@@@@@@@@@@@"
TextF: '13x15
DATA "??@?@?@?@?@??","?@OHOHOHOHO@?","HO@O@O@O@O@O@","HOOOOOOOOOOG@"
DATA "HOOOOOOOOOOG@","HOO@@@O@@OOG@","HOOOOOOOOOOG@","HOO@@@@@@OOG@"
DATA "HOOOOOOOOOOG@","HOO@@@@@@OOG@","HOOOOOOOOOOG@","HOO@@@@@@OOG@"
DATA "HOOOOOOOOOOG@","HGGGGGGGGGGG@","?@@@@@@@@@@@?"
App: '16x14
DATA "HHHHHHHHHHHHHHHH","HGGGGGGGGGGGGGH@","HGAAAAAAAAAAAAH@","HGAAAAAAO@O@O@H@"
DATA "HGHHHHHHHHHHHHH@","HGOOOOOOOOOOOOH@","HGOOOOOOOOOOOOH@","HGOOOOOOOOOOOOH@"
DATA "HGOOOOOOOOOOOOH@","HGOOOOOOOOOOOOH@","HGOOOOOOOOOOOOH@","HGOOOOOOOOOOOOH@"
DATA "HHHHHHHHHHHHHHH@","@@@@@@@@@@@@@@@@"
SysFile:
DATA "HHHHHHHHHH???","HOOOOOOOOGH??","HOOOOOOOOGOH?","HOOOOOOOO@@@@"
DATA "HOOOOOOOOOOG@","HOOFF@OOOOOG@","HOO@NF@OOOOG@","HOFN@HH@OOOG@"
DATA "HOO@G@OH@OOG@","HOOOHO@HH@OG@","HOOOO@GG@OOG@","HOOOOO@@OOOG@"
DATA "HOOOOOOOOOOG@","HGGGGGGGGGGG@","@@@@@@@@@@@@@"
BatFile: '16x14
DATA "HHHHHHHHHHHHHHHH","HGGGGGGGGGGGGGH@","HGAAAAAAAAAAAAH@","HGAAAAAAO@O@O@H@"
DATA "HGHHHHHHHHHHHHH@","HGOOOOOOOOOOOOH@","HGOOOOFF@OOOOOH@","HGOOOO@NF@OOOOH@"
DATA "HGOOOFNHH@FOOOH@","HGOOOO@GN@OOOOH@","HGOOOOO@@OOOOOH@","HGOOOOOOOOOOOOH@"
DATA "HHHHHHHHHHHHHHH@","@@@@@@@@@@@@@@@@"
WavFile: '15x15
DATA "????HHHHHHHH???","????HOOOOOOGH??","????HOOOOOOGOH?","????HFH@OOO@@@@"
DATA "????FO@GHOOOOG@","??FNOH@OHOOHOG@","?NFON@GOHOHOOG@","FOGNO@HOHOOOOG@"
DATA "@NHNO@@OHOHHHG@","?@@FN@GOHOOOOG@","??H@NN@OHOHOOG@","????@FF@HOOHOG@"
DATA "????H@@@OOOOOG@","????HGGGGGGGGG@","????@@@@@@@@@@@"
MpgFile: '16x16
DATA "????HHHHHHHHH???","????HOOOOOOOGH??","????HOOOOOOOGOH?","????HOOOOOOO@@@@"
DATA "????HOOOOOOOOOG@","C???HOOOOOOOOOG@","CCKCCCCCCCOOHHG@","C@@KKK@KKC@CH@G@"
DATA "@?@@@@KKKC@KG@G@","??CKKKKKKC@@C@G@","??CDKK@K@A@O@@G@","??@@@@@@A@@OOOG@"
DATA "????HOOOOOOOOOG@","????HOOOOOOOOOG@","????HGGGGGGGGGG@","????@@@@@@@@@@@@"
ZipFile: '15x16
DATA "??????@@@@@@A??","?LLL?@AIIKKKIA?","LGNGA@@@@@@@IKA","LLLLLLLLLLLO@K@"
DATA "LOOOOGGNFF@O@K@","LOOOGNGNFF@O@K@","LOOOGGNFFF@O@K@","LOOGGGFFFF@O@K@"
DATA "LOGGNNFFFF@O@K@","LOGGNFFFFF@O@K@","L@@@@@@@@@@@IKA","?????@AIIKKKIA?"
DATA "??????@@@@@@A??","??????????@I???","???????@I@I@I@I","???????I@I@I@I@"
FontFile: '13x15
DATA "HHHHHHHHHH???","HOOOOOOOOGH??","HOOOOOOOOGOH?","HOOOOOLOO@@@@"
DATA "HOOOOOLLOOOG@","HOOOOLLLOOOG@","HOOOOLOLLOOG@","HOOOLOOLLOOG@"
DATA "HOOOLLLLLLOG@","HOOLOOOOLLOG@","HOLLLOOLLLLG@","HOOOOOOOOOOG@"
DATA "HOOOOOOOOOOG@","HGGGGGGGGGGG@","@@@@@@@@@@@@@"
IniFile: '13x16
DATA "??@?@?@?@?@??","?@OHOHOHOHO@?","HO@O@O@O@O@O@","HOOOOOOOOOOG@"
DATA "HOOOOOOOOOOG@","HOO@@@O@@OOG@","HOOOOOOOOOOG@","HOO@@@@@@OOG@"
DATA "HOOOOOGFGOOG@","HOO@@GFNF@OG@","HOOOOFN@NFOG@","HOO@@GFNF@OG@"
DATA "HOOOOO@F@OOG@","HOOOOOOOOOOG@","HGGGGGGGGGGG@","?@@@@@@@@@@@?"

REM $STATIC
DEFSNG A-Z
SUB ArrowDown (X1%, Y1%, wdth%, height%, status%, light&, shadow&)
 medium% = wdth% / 2
 LINE (X1%, Y1% - height%)-(X1% + wdth%, Y1% - height%), light&
 LINE (X1%, Y1% - height%)-(X1% + medium%, Y1%), light&
 LINE (X1% + wdth%, Y1% - height%)-(X1% + medium%, Y1%), shadow&
END SUB

SUB ArrowLeft (X1%, Y1%, wdth%, height%, status%, light&, shadow&)
medium% = height% / 2
SELECT CASE status%
CASE 0
LINE (X1%, Y1%)-(X1% - wdth%, Y1% + medium%), shadow&
LINE (X1%, Y1% + height%)-(X1% - wdth%, Y1% + medium%), shadow&
LINE (X1%, Y1%)-(X1%, Y1% + height%), shadow&
CASE 1
LINE (X1%, Y1%)-(X1% - wdth%, Y1% + medium%), light&
LINE (X1%, Y1% + height%)-(X1% - wdth%, Y1% + medium%), shadow&
LINE (X1%, Y1%)-(X1%, Y1% + height%), shadow&
CASE 2
LINE (X1%, Y1%)-(X1% - wdth%, Y1% + medium%), shadow&
LINE (X1%, Y1% + height%)-(X1% - wdth%, Y1% + medium%), light&
LINE (X1%, Y1%)-(X1%, Y1% + height%), shadow&
CASE ELSE
LINE (X1%, Y1%)-(X1% - wdth%, Y1% + medium%), light&
LINE (X1%, Y1% + height%)-(X1% - wdth%, Y1% + medium%), shadow&
LINE (X1%, Y1%)-(X1%, Y1% + height%), shadow&
END SELECT
END SUB

SUB ArrowRight (X1%, Y1%, wdth%, height%, status%, light&, shadow&)
medium% = height% / 2
SELECT CASE status%
CASE 0
LINE (X1%, Y1%)-(X1% + wdth%, Y1% + medium%), shadow&
LINE (X1%, Y1% + height%)-(X1% + wdth%, Y1% + medium%), shadow&
LINE (X1%, Y1%)-(X1%, Y1% + height%), shadow&
CASE 1
LINE (X1%, Y1%)-(X1% + wdth%, Y1% + medium%), light&
LINE (X1%, Y1% + height%)-(X1% + wdth%, Y1% + medium%), shadow&
LINE (X1%, Y1%)-(X1%, Y1% + height%), shadow&
CASE 2
LINE (X1%, Y1%)-(X1% + wdth%, Y1% + medium%), shadow&
LINE (X1%, Y1% + height%)-(X1% + wdth%, Y1% + medium%), light&
LINE (X1%, Y1%)-(X1%, Y1% + height%), shadow&
CASE ELSE
LINE (X1%, Y1%)-(X1% + wdth%, Y1% + medium%), light&
LINE (X1%, Y1% + height%)-(X1% + wdth%, Y1% + medium%), shadow&
LINE (X1%, Y1%)-(X1%, Y1% + height%), shadow&
END SELECT
END SUB

SUB ArrowUp (X1%, Y1%, wdth%, height%, status%, light&, shadow&)
medium% = wdth% / 2
 LINE (X1%, Y1%)-(X1% + medium%, Y1% - height%), light&
 LINE (X1% + medium%, Y1% - height%)-(X1% + wdth%, Y1%), shadow&
 LINE (X1%, Y1%)-(X1% + wdth%, Y1%), shadow&
END SUB

SUB Button (X1%, Y1%, wdth%, height%, bck%, light%, shadow%)
'wdth% = wdth% * 10
'height% = height% * 10
 LINE (X1%, Y1%)-(X1% + wdth%, Y1% + height%), bck%, BF
 LINE (X1%, Y1%)-(X1% + wdth%, Y1% + height%), light%, B
 LINE (X1% + wdth%, Y1% + height%)-(X1% + wdth%, Y1%), shadow%, B
 LINE (X1% + wdth%, Y1% + height%)-(X1%, Y1% + height%), shadow%, B
END SUB

SUB Button2 (X1%, Y1%, wdth%, height%, bck%, light%, shadow%, shadow2%)
 LINE (X1%, Y1%)-(X1% + wdth%, Y1% + height%), bck%, BF
 LINE (X1%, Y1%)-(X1% + wdth%, Y1% + height%), light%, B
 LINE (X1% + wdth%, Y1% + height%)-(X1% + wdth%, Y1%), shadow%, B
 LINE (X1% + wdth%, Y1% + height%)-(X1%, Y1% + height%), shadow%, B
 LINE (X1% + wdth% - 1, Y1% + height% - 1)-(X1% + wdth% - 1, Y1% + 1), shadow2%, B
 LINE (X1% + wdth% - 1, Y1% + height% - 1)-(X1% + 1, Y1% + height% - 1), shadow2%, B
END SUB

SUB CheckForIcons (TheFile$, xd%, yd%)
		EXT$ = TheFile$
		IF INSTR(EXT$, ".BMP") > 0 OR INSTR(EXT$, ".GIF") > 0 OR INSTR(EXT$, ".JPG") > 0 THEN
			DrIcon 3, xd% - 18, yd% - 1
		ELSEIF INSTR(EXT$, ".TXT") > 0 THEN
			DrIcon 4, xd% - 18, yd% - 1
		ELSEIF INSTR(EXT$, ".EXE") > 0 OR INSTR(EXT$, ".COM") > 0 THEN
			DrIcon 5, xd% - 19, yd% - 1
		ELSEIF INSTR(EXT$, ".SYS") > 0 OR INSTR(EXT$, ".DLL") > 0 OR INSTR(EXT$, ".VXD") > 0 THEN
			DrIcon 6, xd% - 19, yd% - 1
		ELSEIF INSTR(EXT$, ".BAT") > 0 THEN
			DrIcon 7, xd% - 19, yd% - 1
		ELSEIF INSTR(EXT$, ".WAV") > 0 THEN
			DrIcon 8, xd% - 19, yd% - 1
		ELSEIF INSTR(EXT$, ".MPG") > 0 OR INSTR(EXT$, ".MP3") > 0 THEN
			DrIcon 9, xd% - 19, yd% - 1
		ELSEIF INSTR(EXT$, ".ZIP") > 0 OR INSTR(EXT$, ".CAB") > 0 THEN
			DrIcon 10, xd% - 19, yd% - 1
		ELSEIF INSTR(EXT$, ".FON") > 0 OR INSTR(EXT$, ".FNT") > 0 OR INSTR(EXT$, ".QBF") > 0 OR INSTR(EXT$, ".TTF") > 0 THEN
			DrIcon 11, xd% - 19, yd% - 1
		ELSEIF INSTR(EXT$, ".INI") > 0 OR INSTR(EXT$, ".INF") > 0 THEN
			DrIcon 12, xd% - 19, yd% - 1
		ELSE
			DrIcon 2, xd% - 18, yd% - 1
		END IF
END SUB

FUNCTION CurrDir$ (Dr$)
	DIM Buffer(128)
	aseg = VARSEG(Buffer(0))
	aptr = VARPTR(Buffer(0))

	IF Dr$ = "" THEN
		Regs.ax = &H1900
		INTERRUPTX &H21, Regs, Regs
		DrCode = (Regs.ax AND 255) + 1
		Dr$ = CHR$(DrCode + 64)
	ELSE
		Dr$ = UCASE$(LEFT$(Dr$, 1))
		DrCode = ASC(Dr$) - 64
	END IF

	Regs.ax = &H4700
	Regs.dx = DrCode
	Regs.ds = aseg
	Regs.si = aptr

	INTERRUPTX &H21, Regs, Regs

	IF Regs.ax = 15 THEN
		ERROR 68       'no such drive
	ELSE
		DEF SEG = aseg
		FOR a = 0 TO 128
			F$ = F$ + CHR$(PEEK(aptr + a))
		NEXT
		DEF SEG

		CurrDir$ = Dr$ + ":\" + LEFT$(F$, INSTR(F$, CHR$(0)) - 1)
	END IF
END FUNCTION

FUNCTION DirX% (Dirspec$, Filter%, SearchType%)
	 IF SearchType% <> FINDFIRST AND SearchType% <> FINDNEXT THEN EXIT FUNCTION
	 IF SearchType% = FINDFIRST THEN
			' SETDTA
			Regs.ax = &H1A00
			Regs.ds = VARSEG(DTA)
			Regs.dx = VARPTR(DTA)
			CALL INTERRUPTX(&H21, Regs, Regs)
	 END IF
	
	 ' Find FIRST or NEXT entry
	 Dirspec$ = Dirspec$ + CHR$(0)
	 Regs.ax = SearchType%
	 Regs.cx = 22
	 Regs.ds = VARSEG(Dirspec$)
	 Regs.dx = SADD(Dirspec$)
	 CALL INTERRUPTX(&H21, Regs, Regs)

	 ' Look after the INT21H call if matches are found
	 IF Regs.flags AND 1 THEN 'is CF set?
			DirX% = NOTMOREFOUND
			EXIT FUNCTION
	 END IF

	 ' Do we have to apply a filter?
	 Result% = TRUE
	 SELECT CASE Filter%
			CASE FILESONLY
				IF DTA.Attributes% = 4096 THEN Result% = FALSE
			CASE DIRSONLY
				IF DTA.Attributes% <> 4096 THEN Result% = FALSE
	 END SELECT
	
	 IF Result% = TRUE THEN
			' Remove the 0 byte that ends up the String in DTA.Filename
			NullByte% = INSTR(DTA.FileName, CHR$(0))
			IF NullByte% > 0 THEN
				DTA.FileName = LEFT$(DTA.FileName, NullByte% - 1) + SPACE$(14 - NullByte%)
			END IF
			DirX% = ENTRYFOUND
	 ELSE
			DirX% = ENTRYNOTMATCHEDFILTER
	 END IF

END FUNCTION

SUB DrIcon (Which%, xr%, yr%)
SELECT CASE Which%
CASE 1
RESTORE Folder
 FOR Y% = 1 TO 13
	 READ Info$
	FOR X% = 1 TO 15
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 2
RESTORE UnknownFile
 FOR Y% = 1 TO 15
	 READ Info$
	FOR X% = 1 TO 13
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 3
RESTORE BMP
 FOR Y% = 1 TO 16
	 READ Info$
	FOR X% = 1 TO 15
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 4
RESTORE TextF
 FOR Y% = 1 TO 15
	 READ Info$
	FOR X% = 1 TO 13
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 5
RESTORE App
 FOR Y% = 1 TO 14
	 READ Info$
	FOR X% = 1 TO 16
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 6
RESTORE SysFile
 FOR Y% = 1 TO 15
	 READ Info$
	FOR X% = 1 TO 13
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 7
RESTORE BatFile
 FOR Y% = 1 TO 14
	 READ Info$
	FOR X% = 1 TO 16
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 8
RESTORE WavFile
 FOR Y% = 1 TO 15
	 READ Info$
	FOR X% = 1 TO 15
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 9
RESTORE MpgFile
 FOR Y% = 1 TO 16
	 READ Info$
	FOR X% = 1 TO 16
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 10
RESTORE ZipFile
 FOR Y% = 1 TO 16
	 READ Info$
	FOR X% = 1 TO 15
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 11
RESTORE FontFile
 FOR Y% = 1 TO 15
	 READ Info$
	FOR X% = 1 TO 13
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
CASE 12
RESTORE IniFile
 FOR Y% = 1 TO 16
	 READ Info$
	FOR X% = 1 TO 13
	 TheClr$ = MID$(Info$, X%, 1): C% = ASC(TheClr$) - 64
	 IF C% <> -1 THEN PSET (xr% + X%, yr% + Y%), C%
 NEXT X%, Y%
END SELECT
END SUB

SUB IniScreen
XK% = 220: YK% = 50
FTV = 15
Button XK%, YK%, (16 * 8) + 65, (FTV * 17) + 70, 7, 15, 0'Dialog
Button2 XK% + 20, YK% + 30, (16 * 8) + 25, (FTV * 17) - 4, 15, 0, 15, 8'White Work Space

'Button2 XK% + 10, YK% + (FTV * 17) + 35, 70, 20, 7, 15, 0, 8'Button 1: Ok
'Button2 XK% + 110, YK% + (FTV * 17) + 35, 70, 20, 7, 15, 0, 8 'Button 2: Cancel

'QFont "OK", XK% + 22, YK% + (FTV * 17) + 36, 0, WinFnt$, 0
'QFont "Cancel", XK% + 119, YK% + (FTV * 17) + 36, 0, WinFnt$, 0

QFont "Enter = Ok", XK% + 22, YK% + (FTV * 17) + 36, 0, WinFnt$, 1
'QFont "Esc = Cancel", XK% + 22, YK% + (FTV * 17) + 52, 0, WinFnt$, 0
QFont "Esc = Cancel", XK% + 100, YK% + (FTV * 17) + 36, 0, WinFnt$, 1

Button2 XK% + 175, YK% + 30, 15, (FTV * 17) - 4, 7, 0, 15, 8'Arrow's Bar
Button XK% + 176, YK% + 31, 13, 14, 7, 15, 0'The Arrow Up Button
ArrowUp XK% + 179, YK% + 41, 6, 6, 1, 0, 0: 'PAINT (XK% + 181, YK% + 40), 0
Button XK% + 176, YK% + (FTV * 17) + 11, 13, 14, 7, 15, 0'The Arrow Down Button
ArrowDown XK% + 179, YK% + (FTV * 17) + 21, 6, 6, 1, 0, 0: 'PAINT (XK% + 182, YK% + 222), 0

'Button XK% + 176, YK% + 47, 12, (FTV * 17) - 38, 7, 15, 0'The Arrow's Bar indicator

Button2 XK% + 3, YK% + 3, 187, 17, 1, 1, 1, 1
QFont "SELECT A BMP FILE TO OPEN", XK% + 15, YK% + 4, 15, WinFnt$, 0

END SUB

FUNCTION OpenFileDialog$ (Drive$, Path$, WhichFileType$)
FTV = 15 'Files To View
DIM File$(1300), Dirs$(400)
SHARED DirsANDFiles$()
DIM DirsANDFiles$(1700)
IniXC% = XK% + 45: IniYC% = YK% + 35: IniYBar% = YK% + 46: IniSzScrll% = (FTV * 17) - 38
WFT$ = WhichFileType$
IniAgain:
HMDir% = 0: HMFile% = 0
FOR j% = 0 TO 1700 - 1
 DirsANDFiles$(j%) = ""
NEXT j%
Found% = DirX%(WFT$, ALLENTRIES, FINDFIRST)
DO WHILE Found% <> NOTMOREFOUND
	 IF Found% = ENTRYFOUND THEN
			IF DTA.Attributes = 4096 THEN
					Dirs$(HMDir%) = DTA.FileName
					HMDir% = HMDir% + 1
			ELSE
					File$(HMFile%) = DTA.FileName
					HMFile% = HMFile% + 1
			END IF
	 END IF
	 Found% = DirX%(WFT$, ALLENTRIES, FINDNEXT)
LOOP

TotalFilesAndDirs% = HMDir% + HMFile%
TotalFilesColumns% = TotalFilesAndDirs% / FTV
OtherFiles& = TotalFilesColumns% * FTV
IF OtherFiles& < TotalFilesAndDirs% THEN TotalFilesColumns% = TotalFilesColumns% + 1

PercColViewed# = 100 / TotalFilesColumns%
SizeOfBar% = (PercColViewed# * IniSzScrll%) / 100

FOR j% = 0 TO HMDir% - 1
 DirsANDFiles$(j%) = Dirs$(j%)
NEXT j%
FOR I% = j% TO j% + (HMFile% - 1)
 DirsANDFiles$(I%) = File$(I% - j%)
NEXT I%

'Button XK% + 176, YK% + 47, 12, (FTV * 17) - 39, 7, 7, 7'The Arrow's Bar indicator
Button2 XK% + 175, YK% + 30, 15, (FTV * 17) - 4, 7, 0, 15, 8'Arrow's Bar
Button XK% + 176, YK% + 31, 13, 14, 7, 15, 0'The Arrow Up Button
ArrowUp XK% + 179, YK% + 41, 6, 6, 1, 0, 0
Button XK% + 176, YK% + (FTV * 17) + 11, 13, 14, 7, 15, 0'The Arrow Down Button
ArrowDown XK% + 179, YK% + (FTV * 17) + 21, 6, 6, 1, 0, 0
Button2 XK% + 20, YK% + 30, (16 * 8) + 25, (FTV * 17) - 4, 15, 0, 15, 8'White Work Space
xd% = IniXC%: yd% = IniYC%
StartFile% = 0: EndFile% = FTV - 1'(ViewedCols% * 12) - 1
FOR L% = StartFile% TO EndFile%
	IF DirsANDFiles$(L%) = "" THEN EXIT FOR
	IF L% >= 0 AND L% < HMDir% THEN
		 DrIcon 1, xd% - 18, yd% + 0
	ELSE
		 CheckForIcons UCASE$(DirsANDFiles$(L%)), xd%, yd%
	END IF
	QFont DirsANDFiles$(L%), xd%, yd%, 0, WinFnt$, 0
	yd% = yd% + 16
	IF (L% + 1) MOD FTV = 0 THEN
		yd% = IniYC%: xd% = xd% + (16 * 8)
	END IF
NEXT L%

C% = 0
XC% = IniXC%: YC% = IniYC%
ColAt% = 0
Button XC% - 1, YC% - 1, 8 * 13, 16, 1, 1, 1
QFont DirsANDFiles$(C%), XC%, YC%, 15, WinFnt$, 0
 Button XK% + 176, IniYBar%, 12, SizeOfBar%, 7, 15, 0'The Arrow's Bar indicator
NowYBar% = IniYBar%
ViewedCol% = 1
oldxc% = XC%: oldyc% = YC%
DO
 a$ = INKEY$

 IF a$ = CHR$(0) + "P" THEN
	 IF C% >= 0 AND C% < TotalFilesAndDirs% - 1 THEN
		KeyDir% = 1: KeyPress% = 1
		C% = C% + 1
	 END IF
 END IF

 IF a$ = CHR$(0) + "H" THEN
	 IF C% > 0 AND C% <= TotalFilesAndDirs% - 1 THEN
		KeyDir% = 2: KeyPress% = 1
		C% = C% - 1
	 END IF
 END IF

 IF KeyPress% = 1 THEN
		IF KeyDir% = 1 THEN ListPos% = 1
		IF KeyDir% = 2 THEN ListPos% = -1
			IF C% MOD FTV <> 0 AND KeyDir% = 1 THEN
				Button oldxc% - 1, oldyc% - 1, 8 * 13, 16, 15, 15, 15
				QFont DirsANDFiles$(C% - ListPos%), oldxc%, oldyc%, 0, WinFnt$, 0
			END IF
			IF C% MOD FTV <> 0 AND KeyDir% = 2 THEN
				Button oldxc% - 1, oldyc% - 1, 8 * 13, 16, 15, 15, 15
				QFont DirsANDFiles$(C% - ListPos%), oldxc%, oldyc%, 0, WinFnt$, 0
			END IF
			IF C% MOD FTV = 0 AND KeyDir% = 2 THEN
				Button oldxc% - 1, oldyc% - 1, 8 * 13, 16, 15, 15, 15
				QFont DirsANDFiles$(C% - ListPos%), oldxc%, oldyc%, 0, WinFnt$, 0
			END IF
			 
				IF C% MOD FTV <> 0 AND KeyDir% = 1 THEN YC% = YC% + (16 * ListPos%)
				IF (C% + 1) MOD FTV <> 0 AND KeyDir% = 2 THEN YC% = YC% + (16 * ListPos%)
				Button XC% - 1, YC% - 1, 8 * 13, 16, 1, 1, 1
				QFont DirsANDFiles$(C%), XC%, YC%, 15, WinFnt$, 0

				oldxc% = XC%: oldyc% = YC%

			IF C% MOD FTV = 0 AND KeyDir% = 1 THEN
				YC% = IniYC%
				Button2 XK% + 20, YK% + 30, (16 * 8) + 25, (FTV * 17) - 4, 15, 0, 15, 8'White Work Space
				xd% = IniXC%: yd% = IniYC%: StartFile% = C%: EndFile% = C% + FTV - 1
				FOR L% = StartFile% TO EndFile%
					IF DirsANDFiles$(L%) = "" THEN EXIT FOR
					IF L% >= 0 AND L% < HMDir% THEN
						DrIcon 1, xd% - 18, yd% + 0
					ELSE
						CheckForIcons UCASE$(DirsANDFiles$(L%)), xd%, yd%
					END IF
					QFont DirsANDFiles$(L%), xd%, yd%, 0, WinFnt$, 0
					yd% = yd% + 16
					IF (L% + 1) MOD FTV = 0 THEN
						yd% = IniYC%: xd% = xd% + (16 * 8)
					END IF
				NEXT L%
				Button XC% - 1, YC% - 1, 8 * 13, 16, 1, 1, 1
				QFont DirsANDFiles$(C%), XC%, YC%, 15, WinFnt$, 0
				Button XK% + 176, NowYBar%, 12, SizeOfBar%, 7, 7, 7'The Arrow's Bar indicator
				'NowYBar% = NowYBar% + SizeOfBar% + 1
				NowYBar% = IniYBar% + (SizeOfBar% * ViewedCol%)
				Button XK% + 176, NowYBar%, 12, SizeOfBar%, 7, 15, 0'The Arrow's Bar indicator
				ViewedCol% = ViewedCol% + 1
				oldxc% = XC%: oldyc% = YC%
			END IF

			IF (C% + 1) MOD FTV = 0 AND KeyDir% = 2 THEN
				YC% = IniYC% + (16 * FTV) - 16
				Button2 XK% + 20, YK% + 30, (16 * 8) + 25, (FTV * 17) - 4, 15, 0, 15, 8'White Work Space
				xd% = IniXC%: yd% = IniYC%: StartFile% = C% - FTV + 1: EndFile% = C%
				FOR L% = StartFile% TO EndFile%
					IF DirsANDFiles$(L%) = "" THEN EXIT FOR
					IF L% >= 0 AND L% < HMDir% THEN
						DrIcon 1, xd% - 18, yd% + 0
					ELSE
						CheckForIcons UCASE$(DirsANDFiles$(L%)), xd%, yd%
					END IF
					QFont DirsANDFiles$(L%), xd%, yd%, 0, WinFnt$, 0
					yd% = yd% + 16
					IF (L% + 1) MOD FTV = 0 THEN
						yd% = IniYC%: xd% = xd% + (16 * 8)
					END IF
				NEXT L%
				Button XC% - 1, YC% - 1, 8 * 13, 16, 1, 1, 1
				QFont DirsANDFiles$(C%), XC%, YC%, 15, WinFnt$, 0
				ViewedCol% = ViewedCol% - 1
				Button XK% + 176, NowYBar%, 12, SizeOfBar%, 7, 7, 7'The Arrow's Bar indicator
				'NowYBar% = NowYBar% - SizeOfBar% - 1
				NowYBar% = IniYBar% + (SizeOfBar% * (ViewedCol% - 1))
				Button XK% + 176, NowYBar%, 12, SizeOfBar%, 7, 15, 0'The Arrow's Bar indicator
				oldxc% = XC%: oldyc% = YC%
			END IF
		KeyPress% = 0
 END IF

 IF a$ = CHR$(27) THEN OpenFileDialog$ = "ERROR": ERASE DirsANDFiles$: EXIT FUNCTION
 IF a$ = CHR$(13) THEN 'OpenFileDialog$ = DirsANDFiles$(c%): ERASE DirsANDFiles$: EXIT FUNCTION
	IF C% >= 0 AND C% < HMDir% THEN
		NowDir$ = LTRIM$(RTRIM$(DirsANDFiles$(C%)))
		'SHELL "cd " + NowDir$
		CHDIR NowDir$
		'LOCATE 1, 1: PRINT NowDir$
		GOTO IniAgain
	ELSE
		OpenFileDialog$ = LTRIM$(RTRIM$(DirsANDFiles$(C%)))
		ERASE DirsANDFiles$
		EXIT FUNCTION
	END IF
 END IF
LOOP
END FUNCTION


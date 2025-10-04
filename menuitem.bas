'** If you are reading this message you loaded the wrong module
'** It doesn't do anything by itself.
'** PLEASE, In order to run the full application you need to
'** to load QUICKPT4.BAS....

DECLARE SUB Item.Save ()
DECLARE FUNCTION TextBox$ (xpos%, ypos%, W%, h%, Font$, MaxChar%)
DECLARE SUB QFont (txt$, x%, y%, C%, FontFile$, Attribs%)
DECLARE SUB LoadBmp (Name$, x1%, y1%)
DECLARE SUB PutUndoMem ()
DECLARE FUNCTION CurrDir$ (Dr$)
DECLARE SUB IniScreen ()
DECLARE FUNCTION OpenFileDialog$ (Drive$, Path$, WhichFileType$)


DECLARE SUB ReDrawObjects ()
DECLARE SUB DrawLabel (WinID%, Obj%, x1%, y1%, W%, h%, Caption$)
DECLARE FUNCTION DrawButton% (WinID%, ID%, x1%, y1%, W%, h%, Caption$)
DECLARE SUB Save640 (File$)
DECLARE SUB Load640 (File$)
DECLARE SUB KillDialog (ID%)
DEFINT A-Z
DECLARE SUB MouseStatus (mb%, x%, y%)
DECLARE SUB DrawDialog (ID%, x%, y%, W%, h%, Caption$)
DECLARE SUB WinFont (txt$, x%, y%, Clr%, bold%)
DECLARE SUB SaveScr (FileName$, sx%, sy%, ex%, ey%, nbits%, imgnum%)
DECLARE SUB Mouse (Action%)
DECLARE SUB FButon (x1%, y1%, W%, h%, bck%, light%, shadow%, shadow2%)
'**************************************************************************
CONST Show = 1, Hide = 2 'Constants for Mouse SUB
COMMON SHARED PAX1%, PAY1%, PAX2%, PAY2% 'Paint Area Coords
COMMON SHARED BackClr%, ForeClr%, TheImgName$, FileModified%
COMMON SHARED TheFontPath$, StdFnt$, WinFnt$, SwapVideoFile$

DEFSNG A-Z
SUB Item.ClearImg
  Mouse Hide
  'EreaseClr% = 15
  EreaseClr% = BackClr%
  FButon PAX1% + 1, PAY1% + 1, PAX2% - PAX1% - 3, PAY2% - PAY1% - 3, EreaseClr%, EreaseClr%, EreaseClr%, EreaseClr%
  Mouse Show
END SUB

DEFINT A-Z
SUB Item.InvertColrs
 'FButon PAX1% + 1, PAY1% + 1, PAX2% - PAX1% - 3, PAY2% - PAY1% - 3, EreaseClr%, EreaseClr%, EreaseClr%, EreaseClr%
 ArraySize& = 4 + INT(((PAX2% - PAX1% + 1) * (1) + 7) / 8) * 4 * ((PAY2% - PAY1%) + 1)
 'PRINT ArraySize&
 TrueArraySize& = ArraySize& / 4
 'PRINT TrueArraySize&
 DIM Cube1(TrueArraySize&), Cube2(TrueArraySize&)
 theY2% = (PAY2% - PAY1%) / 2
 Mouse Hide
   GET (PAX1% + 1, PAY1% + 1)-(PAX2% - 2, PAY1% + theY2%), Cube1
   GET (PAX1% + 1, PAY1% + theY2% + 1)-(PAX2% - 2, PAY2% - 2), Cube2
   PUT (PAX1% + 1, PAY1% + 1), Cube1, PRESET
   PUT (PAX1% + 1, PAY1% + theY2% + 1), Cube2, PRESET
 Mouse Show
 ERASE Cube1, Cube2
END SUB

DEFSNG A-Z
SUB Item.New
'  IF FileModified% = 1 THEN
    Mouse Hide
     FButon PAX1%, PAY1%, PAX2% - PAX1%, PAY2% - PAY1%, BackClr%, 0, 15, 8
     'FileModified% = 0
     BMPName$ = "UNTITLED.BMP"
     TheImgName$ = CurrDir$("C:") + "\" + BMPName$
     FButon 150, 4, 380, 15, 1, 1, 1, 1
     QFont "- " + TheImgName$, 150, 3, 15, StdFnt$, 0
    Mouse Show
'  END IF
END SUB

DEFINT A-Z
SUB Item.Open
Mouse Hide
 Save640 SwapVideoFile$
  IniScreen
  'DrawDialog 1, 10, 10, 20, 30
  FileSelected$ = OpenFileDialog$("C:", CurrDir$("C:"), "*.*")
 Load640 SwapVideoFile$
 'LINE (PAX1% + 2, PAY1% + 2)-(PAX1% + 2, PAY1% + 2), BackClr%
  IF FileSelected$ <> "ERROR" THEN
     IF INSTR(FileSelected$, ".BMP") <> 0 THEN
       TheImgName$ = CurrDir$("C:") + "\" + FileSelected$
       FButon PAX1%, PAY1%, PAX2% - PAX1%, PAY2% - PAY1%, BackClr%, 0, 15, 8
       'LoadBmp TheImgName$, 120, 50
       LoadBmp TheImgName$, PAX1% + 1, PAY1% + 1
       FButon 150, 4, 380, 15, 1, 1, 1, 1
       QFont "- " + TheImgName$, 150, 3, 15, StdFnt$, 0
     ELSE
       'PRINT "File "; FileSelected$; " is not a BMP File"
      FButon 200, 50, 250, 150, 7, 15, 8, 0
      FButon 203, 53, 244, 18, 1, 1, 1, 1
      QFont "ERROR!", 210, 53, 15, StdFnt$, 0
      QFont "File " + FileSelected$ + " is not a valid", 205, 80, 0, StdFnt$, 0
      QFont "BMP file!", 205, 96, 0, StdFnt$, 0
      QFont "Press any key to continue...", 205, 112 + 16, 0, StdFnt$, 1
      SLEEP
      Load640 SwapVideoFile$
     END IF
  END IF
Mouse Show
 KILL SwapVideoFile$ + ".BLU"
 KILL SwapVideoFile$ + ".GRN"
 KILL SwapVideoFile$ + ".RED"
 KILL SwapVideoFile$ + ".INT"
END SUB

DEFSNG A-Z
SUB Item.Save
  FButon 10, 455, 620, 18, 7, 0, 15, 8
  WinFont "Saving...", 23, 458, 0, 1
  Mouse Hide
  SaveScr TheImgName$, PAX1% + 1, PAY1% + 1, PAX2% - 2, PAY2% - 2, 8, 0
  FButon 10, 455, 620, 18, 7, 0, 15, 8
  WinFont "File " + TheImgName$ + " succesfully saved!", 23, 458, 0, 1
  Mouse Show
END SUB

DEFINT A-Z
SUB Item.SaveAs

Mouse Hide
 Save640 SwapVideoFile$

  FButon 200, 50, 250, 150, 7, 15, 8, 0
  FButon 203, 53, 244, 18, 1, 1, 1, 1
  QFont "SAVE AS...", 210, 53, 15, StdFnt$, 0
  QFont "Write a new name to save this draw", 205, 80, 0, StdFnt$, 0
  QFont "in the current directory:", 205, 96, 0, StdFnt$, 0
  QFont LCASE$(CurrDir$("C:")) + "\", 205, 112, 0, StdFnt$, 0
  'WinFont LCASE$(CurrDir$("C:")) + "\", 205, 112, 0, 0
  TheNewName$ = TextBox$(250, 150, 120, 20, StdFnt$, 12)
  IF TheNewName$ = "" THEN
    Load640 SwapVideoFile$  '------------------------------------------
    GOTO endsaveas1
  END IF
  IF TheNewName$ <> "ERROR!" THEN
    IF INSTR(TheNewName$, ".") = 0 THEN TheNewName$ = TheNewName$ + ".BMP"
   Load640 SwapVideoFile$  '------------------------------------------
    TheImgName$ = TheNewName$
    Item.Save
     TheImgName$ = CurrDir$("C:") + "\" + TheNewName$
     FButon 150, 4, 380, 15, 1, 1, 1, 1
     QFont "- " + TheImgName$, 150, 3, 15, StdFnt$, 0
  ELSE
   Load640 SwapVideoFile$  '------------------------------------------
  END IF

 'PRINT TheNewName$'ERROR!
endsaveas1:
Mouse Show
 KILL SwapVideoFile$ + ".BLU"
 KILL SwapVideoFile$ + ".GRN"
 KILL SwapVideoFile$ + ".RED"
 KILL SwapVideoFile$ + ".INT"
END SUB

SUB Item.Settings
 
END SUB

SUB Item.Undo
 PutUndoMem
END SUB


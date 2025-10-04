v# 🖌️ Quick Paint (QuickBasic 4.5)

![Platform](https://img.shields.io/badge/platform-DOS%20%2F%20FreeDOS-blue)
![Language](https://img.shields.io/badge/language-QuickBasic%204.5-purple)
![License](https://img.shields.io/badge/license-MIT-green)
![Status](https://img.shields.io/badge/status-Alpha%200.4%20Build%20271005-orange)

> A retro paint program written in **QuickBasic 4.5**, designed for DOS and compatible VGA environments.  
> Developed by **Jonathan Gallegos (JG)** — a modern revival of classic DOS creativity tools.

---

## ✨ Overview

**Quick Paint** is a complete paint environment for DOS, using **SCREEN 12 (640×480, 16 colors)**.  
It features a windowed interface, menus, dialogs, and a full suite of drawing tools inspired by MS Paint — but entirely coded in **QuickBasic 4.5**.

![Quick Paint — SCREEN 12 UI](<img width="935" height="704" alt="image" src="https://github.com/user-attachments/assets/8a39c89a-5f0c-4956-95b5-4ace79c58b1b" />
)

---


## 📦 Downloads
[![Latest Release](https://img.shields.io/github/v/release/<user>/<repo>?label=Download%20Latest)](https://github.com/jgallegos-acosta/quickpaint-dos/releases/tag/Release)


[![Download QPAINT040.ZIP](https://img.shields.io/badge/Download-QPAINT040.ZIP-blue?style=for-the-badge)](https://github.com/jgallegos-acosta/quickpaint-dos/releases/download/Release/QPAINT040.zip)


- **Quick Paint for FreeDOS** — get the latest ZIP from the Releases page.
- Checksums (SHA-256) will be attached as assets per release.

> Replace `<user>/<repo>` above with your GitHub owner and repository.

---

## 🧱 Project Structure

```
QUICKPT4.BAS   → Main program: UI core, menu system, tool dispatcher, and initialization.
TOOLS.BAS      → Drawing tools (Pencil, Brush, AirBrush, Line, Rectangle, Ellipse, RoundedBox, Polygon, Curve, Fill, Dropper, Eraser, Text).
GUI.BAS        → Windowing, menu bar, buttons, dialogs, mouse helpers, and QFont text routines.
MENUITEM.BAS   → Menu actions (New, Open, Save, Undo, Invert Colors, Clear Image, Exit).
OD1B.BAS       → File Open Dialog (INT 21h FindFirst/FindNext) with file/folder icons.
```

---

## 🧰 Features

### 🎨 Drawing Tools
Each tool is mouse-driven, with left/right buttons for Foreground/Background colors:

- **Pencil**
- **Brush**
- **AirBrush**
- **Line**
- **Rectangle**
- **Ellipse**
- **RoundedBox**
- **Polygon**
- **Curve** (Bezier routine)
- **Fill**
- **Dropper**
- **Eraser**
- **Text**

### 🪟 Interface
- Custom **windowed GUI** with title bar, menu bar, and status bar.
- **Menus:** File, Edit, View, Image, Colors, Help.
- **File Dialog:** DOS-style Open/Save using directory traversal and icons.
- **Undo:** Single-level undo with temp files (`.RED`, `.GRN`, `.BLU`, `.INT`).

### 💾 Image I/O
- **Open / Save / Save As**: reads and writes `.BMP` with VGA-compatible palettes.
- **Clear Image**, **Invert Colors**, and **Undo** supported.

### 🔤 QFont Rendering
- Uses external `.FNT` raster font files (`lucidabl.fnt`, `verdana.fnt`, etc.).
- Font rendering via QFont (by Josh Heaton).
- Supports basic inline formatting toggles.

---


## 📁 Package Contents

The official release archive **QPAINT040.ZIP** includes:

```
QPAINT040.ZIP
 ├─ QPAINT.COM / QPAINT.EXE
 ├─ README.TXT
 ├─ LICENSE.TXT
 ├─ FILE_ID.DIZ
 ├─ CREDITS.TXT
 ├─ DOCS/
 │   └─ README.md
 ├─ FONTS/     ← includes default QFont `.FNT` files
 └─ BMP/       ← demo BMP files for testing Open/Save
```

## 🚀 Getting Started (DOSBox + QuickBasic 4.5)

### Requirements
- **MS-DOS** or **FreeDOS** (works perfectly under **DOSBox**)
- **QuickBasic 4.5**
- **Mouse driver** (INT 33h)
- `QB.BI` and `QB.QLB` from your QB installation

### Steps

1. Place all `.BAS` files in a single folder. The official package already includes the `FONTS/` folder with `.FNT` fonts and `BMP/` demos.
2. Adjust the font path in `TOOLS.BAS` → `SUB Tool.Text`:
   ```basic
   ActualFont$ = "C:\YOUR\FONTS\PATH\lucidabl.fnt"
   ' or:
   ActualFont$ = TheFontPath$ + "lucidabl.fnt"
   ```
3. Launch QuickBasic:
   ```dos
   QB.EXE /L QB.QLB QUICKPT4.BAS
   ```
4. Add the other `.BAS` files: `TOOLS.BAS`, `GUI.BAS`, `MENUITEM.BAS`, `OD1B.BAS`.
5. Run with **Shift+F5** or build with **Alt+F, M**.

### Command-line build
```dos
BC.EXE QUICKPT4.BAS+TOOLS.BAS+GUI.BAS+MENUITEM.BAS+OD1B.BAS /O;
LINK.EXE QUICKPT4+TOOLS+GUI+MENUITEM+OD1B,,NUL, QB.LIB;
```

---

## 🖱️ How to Use

| Action | Description |
|--------|--------------|
| **Left Click** | Draw with Foreground color |
| **Right Click** | Draw with Background color |
| **Toolbox (left)** | Select drawing tool |
| **Menu bar** | Access File / Edit / Image / Color actions |
| **Text Tool** | Drag a box, type, and press Enter |
| **Undo** | Single-level (temporary buffer) |
| **Invert Colors** | Swap palette entries for artistic effects |

Canvas is clipped to defined boundaries (`PAX1%`, `PAY1%`, `PAX2%`, `PAY2%`) to avoid overwriting UI.

---

## 💾 File Format

- Saves standard **BMP 8-bit headers** with a **16-color VGA palette**.
- Undo uses intermediate memory files (`memscr.*`).
- Open Dialog (OD1B) uses DOS interrupts for directory reading.

---

## ⚠️ Known Limitations

- VGA **SCREEN 12** only.
- **Single Undo** level.
- Zoom and advanced Image Settings not yet implemented.
- Some paths are hardcoded (font directory, temp files).
- Text tool requires manual font configuration.

---

## 🤝 Credits

| Component | Author |
|------------|---------|
| QFont Engine | Josh Heaton |
| SaveBMP Routine | Aaron Zabudsky |
| Bezier Curve | Jim Emptage |
| Project Lead | **Jonathan Gallegos (JG)** |
| Contact | linuxloader815@gmail.com |

> Original source headers: “By JG — Quick Paint Project.”

---

## 🧭 Roadmap

- [ ] Multi-level Undo / Redo  
- [ ] Zoom and Pan  
- [ ] PNG Export  
- [ ] Palette Editor  
- [ ] Brush Patterns / Dithering  
- [ ] Clipboard Copy & Paste  
- [ ] Modern QB64 port  

---

## 📜 License

Released under the **MIT License**.  
See [`LICENSE`](LICENSE) for details.

```text
MIT License © 2025 Jonathan Gallegos
Permission is hereby granted, free of charge, to any person obtaining a copy of this software...
```

---

## 🏁 Quick Demo Checklist

1. **New File** → Draw shapes and text  
2. **Save As** → `test.bmp`  
3. **Undo**, then **Invert Colors**  
4. **Save** again  
5. **Reopen** to verify persistence  

🎨 *Enjoy painting on FreeDOS!*  
If you’d like to see Quick Paint included officially, consider opening a [FreeDOS Program Proposal](https://www.freedos.org/software/) referencing this repository.

---

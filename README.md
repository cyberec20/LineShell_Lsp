# 📐 LineShell.LSP – Symmetric Offset and Hatch Automation for AutoCAD

**LineShell.LSP** is an AutoLISP routine for AutoCAD that automates the process of creating closed contours from open lines or polylines, by generating symmetric offsets, closing the shape, applying hatches, and optionally grouping the result as a block.

---

## 🔧 Features

- ✨ **Symmetric offset** generation on both sides of selected lines or polylines.
- 🧱 **Automatic closing** of open ends with line segments.
- 📏 **Polyline join** into a closed contour.
- 🎨 **Hatch fill** using a selectable pattern (`SOLID`, `ANSI31`, etc.).
- 📁 **Block creation** option with newly generated geometry.
- 📂 Moves original objects to `Defpoints` layer with predefined style (gray, dashed).
- ✅ Supports only `LINE` and `LWPOLYLINE` types for safety and consistency.

---

## 🎯 Use Cases

- Quick drafting of **trench outlines**, **ducts**, **walls**, or **infrastructure cross-sections**.
- Automating **visual enhancement** and **hatching** for documentation or modeling.
- Reducing manual repetitive tasks in **civil**, **electrical**, or **architectural** drawings.

---

## ▶️ How to Use

1. Load the file with `APPLOAD` in AutoCAD.
2. Run the command by typing: `LineShell`.
3. Select lines or polylines (open or closed).
4. Enter the **offset distance** (half-width from centerline).
5. Choose a hatch pattern if desired.
6. Choose whether to create a block.

> ⚠️ Only works with valid, non-degenerate geometry. Ensure the input is clean.

---

## 📄 License

Released under the **GNU 3.0 License** – see [`LICENSE`](LICENSE) for details.

---

## 👤 Author

Developed by [Franklin Rodríguez](https://www.linkedin.com/in/franklinrodriguezacosta)

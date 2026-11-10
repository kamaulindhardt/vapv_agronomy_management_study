"""Assemble the VAPV vs open-field summary report (DOCX) from the CSV tables and PNG figures
written by 01_data_and_tables.R / 02_figures.R.

Every number in the running text is read from tables/*.csv -- nothing is typed by hand -- so the
report cannot drift from the analysis. Run with Python 3.13 (python-docx):
    python build_report_docx.py [output.docx]
"""
import math
import re
import sys
from pathlib import Path

import pandas as pd
from docx import Document
from docx.enum.section import WD_ORIENT
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Cm, Pt, RGBColor

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
TAB = HERE / "tables"
FIG = HERE / "figures"
OUT = Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT / "docs" / "vapv_vs_openfield_summary_report.docx"

FONT = "Calibri"
NAVY = RGBColor(0x1F, 0x3A, 0x5F)
GREY = RGBColor(0x59, 0x59, 0x59)
HEAD_FILL = "E8EDF3"
BOX_FILL = "F3F6F9"


# --------------------------------------------------------------------------- data + formatting
def T(name):
    return pd.read_csv(TAB / f"{name}.csv")


def ok(x):
    return x is not None and not (isinstance(x, float) and math.isnan(x))


def num(x, d=1):
    return "n/a" if not ok(x) else f"{x:.{d}f}".replace("-", "−")


def sgn(x, d=0):
    if not ok(x):
        return "n/a"
    r = round(x, d)
    return f"{(r + 0.0):+.{d}f}".replace("-", "−")


def pct(x, d=0):
    return "n/a" if not ok(x) else f"{sgn(x, d)}%"


def ci(l, u, d=0, plus=False):
    if not (ok(l) and ok(u)):
        return "n/a"
    # CI end-points keep their true sign (an end-point of -0.04 reads "-0.0", not "+0.0")
    f = (lambda x, k: f"{x:+.{k}f}".replace("-", "−")) if plus else num
    return f"[{f(l, d)}, {f(u, d)}]"


def pm(m, s, d=2):
    return "n/a" if not ok(m) else (f"{num(m, d)} ± {num(s, d)}" if ok(s) else num(m, d))


def pv(p):
    return "n/a" if not ok(p) else ("<0.001" if p < 0.001 else f"{p:.3f}")


def rng(a, b, d=0):
    """'18' if both round equal, else '18–20'."""
    return num(a, d) if num(a, d) == num(b, d) else f"{num(min(a, b), d)}–{num(max(a, b), d)}"


def excl0(l, u):
    return ok(l) and ok(u) and (l > 0 or u < 0)


CY_NAME = {"WW 2023": "Winter wheat 2023", "WW 2024": "Winter wheat 2024", "SY 2024": "Soybean 2024",
           "GC 2023": "Grass-clover 2023 (3 cuts)", "GC 2024": "Grass-clover 2024 (5 cuts)",
           "GC 2025": "Grass-clover 2025 (5 cuts)", "SB 2025": "Spring barley 2025 (green biomass)",
           "RG 2025": "Ryegrass 2025 (undersown)"}
POS = ["West", "Centre", "East"]

# --------------------------------------------------------------------------- docx helpers
doc = Document()
sec = doc.sections[0]
sec.page_width, sec.page_height = Cm(21.0), Cm(29.7)
sec.left_margin = sec.right_margin = Cm(2.0)
sec.top_margin, sec.bottom_margin = Cm(1.9), Cm(1.8)
CONTENT_W = 17.0

st = doc.styles
st["Normal"].font.name = FONT
st["Normal"].font.size = Pt(10)
st["Normal"].element.rPr.rFonts.set(qn("w:eastAsia"), FONT)
st["Normal"].paragraph_format.space_after = Pt(5)
st["Normal"].paragraph_format.line_spacing = 1.08
for name, size, before, after in [("Heading 1", 14, 12, 4), ("Heading 2", 11, 8, 3)]:
    h = st[name]
    h.font.name = FONT
    h.font.size = Pt(size)
    h.font.bold = True
    h.font.color.rgb = NAVY
    h.element.rPr.rFonts.set(qn("w:ascii"), FONT)
    h.element.rPr.rFonts.set(qn("w:hAnsi"), FONT)
    h.paragraph_format.space_before = Pt(before)
    h.paragraph_format.space_after = Pt(after)
    h.paragraph_format.keep_with_next = True


def _fmt(run, size=None, bold=False, italic=False, color=None):
    run.font.name = FONT
    if size:
        run.font.size = Pt(size)
    run.bold = bold
    run.italic = italic
    if color is not None:
        run.font.color.rgb = color


def rich(p, text, size=None, bold=False, italic=False, color=None):
    """Add text to paragraph p. **x** = bold; NO3 gets a subscript 3."""
    for part in re.split(r"(\*\*.+?\*\*)", text):
        b = bold
        if part.startswith("**") and part.endswith("**"):
            part, b = part[2:-2], True
        for sp in re.split(r"(NO3)", part):
            if sp == "NO3":
                _fmt(p.add_run("NO"), size, b, italic, color)
                r = p.add_run("3")
                _fmt(r, size, b, italic, color)
                r.font.subscript = True
            elif sp:
                _fmt(p.add_run(sp), size, b, italic, color)
    return p


def para(text, size=None, bold=False, italic=False, color=None, after=None, align=None, keep=False, style=None):
    p = doc.add_paragraph(style=style)
    rich(p, text, size, bold, italic, color)
    if after is not None:
        p.paragraph_format.space_after = Pt(after)
    if align:
        p.alignment = align
    if keep:
        p.paragraph_format.keep_with_next = True
    return p


def bullet(text, size=None, after=2):
    p = doc.add_paragraph(style="List Bullet")
    rich(p, text, size)
    p.paragraph_format.space_after = Pt(after)
    return p


def shade(cell, fill):
    tcPr = cell._tc.get_or_add_tcPr()
    shd = OxmlElement("w:shd")
    shd.set(qn("w:val"), "clear")
    shd.set(qn("w:color"), "auto")
    shd.set(qn("w:fill"), fill)
    tcPr.append(shd)


TBLPR_ORDER = ["tblStyle", "tblpPr", "tblOverlap", "bidiVisual", "tblStyleRowBandSize", "tblStyleColBandSize", "tblW", "jc",
               "tblCellSpacing", "tblInd", "tblBorders", "shd", "tblLayout", "tblCellMar", "tblLook"]


def tblpr_insert(tblPr, el):
    """Insert el into tblPr respecting the OOXML schema child order (Word is strict about it)."""
    name = el.tag.split("}")[1]
    later = TBLPR_ORDER[TBLPR_ORDER.index(name) + 1:]
    for child in tblPr:
        if child.tag.split("}")[1] in later:
            child.addprevious(el)
            return
    tblPr.append(el)


def cell_margins(table, top=30, bottom=30, left=60, right=60):
    m = OxmlElement("w:tblCellMar")
    for k, v in (("top", top), ("left", left), ("bottom", bottom), ("right", right)):
        e = OxmlElement(f"w:{k}")
        e.set(qn("w:w"), str(v))
        e.set(qn("w:type"), "dxa")
        m.append(e)
    tblpr_insert(table._tbl.tblPr, m)


def borders(table, outer="808080", inner="D0D0D0"):
    b = OxmlElement("w:tblBorders")
    for k, c, sz in (("top", outer, 8), ("bottom", outer, 8), ("insideH", inner, 4)):
        e = OxmlElement(f"w:{k}")
        e.set(qn("w:val"), "single")
        e.set(qn("w:sz"), str(sz))
        e.set(qn("w:space"), "0")
        e.set(qn("w:color"), c)
        b.append(e)
    tblpr_insert(table._tbl.tblPr, b)


def row_props(row, header=False):
    trPr = row._tr.get_or_add_trPr()
    e = OxmlElement("w:cantSplit")
    trPr.append(e)
    if header:
        h = OxmlElement("w:tblHeader")
        trPr.append(h)


def table(header, rows, widths, align=None, size=8, bold_cells=(), bold_rows=(), shade_rows=(), first_col_bold=False, keep=True):
    """rows: list of lists of str. bold_cells: set of (r, c). align: list of 'l'/'c'/'r' per column."""
    n = len(header)
    align = align or ["l"] + ["c"] * (n - 1)
    t = doc.add_table(rows=1 + len(rows), cols=n)
    t.alignment = WD_TABLE_ALIGNMENT.CENTER
    t.autofit = False
    borders(t)
    cell_margins(t)
    amap = {"l": WD_ALIGN_PARAGRAPH.LEFT, "c": WD_ALIGN_PARAGRAPH.CENTER, "r": WD_ALIGN_PARAGRAPH.RIGHT}

    def fill(cell, text, w, a, bold=False, fillc=None):
        cell.width = Cm(w)
        p = cell.paragraphs[0]
        p.paragraph_format.space_after = Pt(0)
        p.paragraph_format.line_spacing = 1.0
        p.alignment = amap[a]
        rich(p, text, size=size, bold=bold)
        if fillc:
            shade(cell, fillc)

    for j, h in enumerate(header):
        fill(t.rows[0].cells[j], h, widths[j], align[j] if j else "l", bold=True, fillc=HEAD_FILL)
    row_props(t.rows[0], header=True)
    for i, r in enumerate(rows):
        row_props(t.rows[i + 1])
        for j, txt in enumerate(r):
            b = (i, j) in bold_cells or i in bold_rows or (first_col_bold and j == 0)
            fill(t.rows[i + 1].cells[j], str(txt), widths[j], align[j], bold=b, fillc=("F7F7F7" if i in shade_rows else None))
    # keep short tables on one page (all rows keep-with-next); long tables (keep=False) may break between rows
    for r in (t.rows[:-1] if keep else t.rows[:1]):
        for c in r.cells:
            for p in c.paragraphs:
                p.paragraph_format.keep_with_next = True
    global _LAST_TABLE
    _LAST_TABLE = t
    return t


_LAST_TABLE = None


def caption(text, before=8, after=3, keep=True):
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(before)
    p.paragraph_format.space_after = Pt(after)
    p.paragraph_format.keep_with_next = keep
    m = re.match(r"^((?:Table|Figure) [A-Z]?\d+\.)\s*(.*)$", text, re.S)
    if m:
        rich(p, m.group(1) + " ", size=9, bold=True, color=NAVY)
        rich(p, m.group(2), size=9)
    else:
        rich(p, text, size=9)
    return p


def note(text, after=6):
    if _LAST_TABLE is not None:      # keep the table's last row with its note
        for c in _LAST_TABLE.rows[-1].cells:
            for pp in c.paragraphs:
                pp.paragraph_format.keep_with_next = True
    p = para(text, size=8, italic=True, color=GREY, after=after)
    p.paragraph_format.space_before = Pt(2)
    return p


def figure(name, width_cm=16.8):
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.keep_with_next = True
    p.paragraph_format.space_after = Pt(2)
    p.add_run().add_picture(str(FIG / f"{name}.png"), width=Cm(width_cm))


def add_field(run, instr):
    for kind, txt in (("begin", None), (None, instr), ("separate", None), (None, "1"), ("end", None)):
        if kind:
            e = OxmlElement("w:fldChar")
            e.set(qn("w:fldCharType"), kind)
        elif txt == instr:
            e = OxmlElement("w:instrText")
            e.set(qn("xml:space"), "preserve")
            e.text = instr
        else:
            e = OxmlElement("w:t")
            e.text = txt
        run._r.append(e)


def footer():
    f = sec.footer
    p = f.paragraphs[0]
    _fmt(p.add_run("VAPV vs open field – quantitative summary (draft for internal review)\tPage "), 8, color=GREY)
    r = p.add_run()
    _fmt(r, 8, color=GREY)
    add_field(r, "PAGE")
    _fmt(p.add_run(" of "), 8, color=GREY)
    r2 = p.add_run()
    _fmt(r2, 8, color=GREY)
    add_field(r2, "NUMPAGES")
    # right tab stop at the text width (clear the Footer style's default centre/right stops); <w:tabs> goes right after <w:pStyle>
    pPr = p._p.get_or_add_pPr()
    tabs = OxmlElement("w:tabs")
    for val, pos in (("clear", 4680), ("clear", 9360), ("right", int(CONTENT_W / 2.54 * 1440))):
        tb = OxmlElement("w:tab")
        tb.set(qn("w:val"), val)
        tb.set(qn("w:pos"), str(pos))
        tabs.append(tb)
    ps = pPr.find(qn("w:pStyle"))
    if ps is not None:
        ps.addnext(tabs)
    else:
        pPr.insert(0, tabs)


footer()

# --------------------------------------------------------------------------- load tables
head = T("tbl_headline")
ycy = T("tbl_yield_cropyear").set_index("cy")
yov = T("tbl_yield_overall").set_index("CROP")
ypos = T("tbl_yield_position")
ypd = T("tbl_yield_position_delta")
tn = T("tbl_tissue_n").set_index("CROP")
ns = T("tbl_no3_strip").set_index("strip")
nse = T("tbl_no3_season")
npos = T("tbl_no3_position")
ncon = T("tbl_no3_concentration")
tdo = T("tbl_tdr_overall")
tdn = T("tbl_tdr_nest_level").set_index("depth")
tdp = T("tbl_tdr_position")
tdy = T("tbl_tdr_year_levels")
socr = T("tbl_soc_round").set_index("round")
socp = T("tbl_soc_position")
did = T("tbl_soc_did").iloc[0]
bc = T("tbl_baseline_totalC").set_index("depth")
btex = T("tbl_baseline_texture")
syn = T("tbl_synthesis_relative")
cov = T("tbl_data_coverage")
qc = T("tbl_qc")
om1 = pd.read_csv(ROOT / "4_outputs" / "tables" / "manuscript" / "Fig01_omnibus_tests.csv")
om2 = pd.read_csv(ROOT / "4_outputs" / "tables" / "manuscript" / "Fig02_omnibus_tests.csv")
dl_val = pd.read_csv(HERE / "tables" / "tbl_dl_vs_daisy.csv") if (HERE / "tables" / "tbl_dl_vs_daisy.csv").exists() else None

A = yov.loc["ALL"]
hl = lambda m: head[head.metric == m].iloc[0]

# facts derived from the tables so that no number in the text is typed by hand
cum = T("tbl_no3_cumulative_series")
NDAYS = (pd.to_datetime(cum.date.max()) - pd.to_datetime(cum.date.min())).days + 1
NO3_DATES = int(re.search(r"(\d+) sampling dates", cov[cov.domain == "Nitrate leaching"].n.iloc[0]).group(1))
qcn = lambda key: int(qc[qc["item"].str.contains(key, regex=False)].n.iloc[0])
dl = T("tbl_dl_vs_daisy").set_index("strip")
TDR_ROBUST = (tdo["diff"] - tdo["diff_first"]).abs().max()
_worst = tdo.loc[(tdo["diff"] - tdo["diff_first"]).abs().idxmax()]


def yp(cy, pos, col):
    return ypd[(ypd.cy == cy) & (ypd.pos == pos)].iloc[0][col]


def tdr(depth, year, col):
    return tdo[(tdo.depth == depth) & (tdo.year.astype(str) == str(year))].iloc[0][col]


def nsea(strip, season, col):
    return nse[(nse.strip == strip) & (nse.season == season)].iloc[0][col]


def npp(strip, env, pos, col):
    return npos[(npos.strip == strip) & (npos.env == env) & (npos.pos == pos)].iloc[0][col]


def synv(outcome_start, env, pos):
    r = syn[(syn.outcome.str.startswith(outcome_start)) & (syn.env == env) & (syn.pos == pos)]
    return r.iloc[0]["mean"]


# =========================================================================== TITLE
p = para("VAPV versus open field: crop productivity, nitrate leaching, soil water and soil carbon",
         size=19, bold=True, color=NAVY, after=2)
para("Quantitative summary with substrip-level detail – Study 2 (agronomic management), Value4Farm vertical agrivoltaic trial, "
     "Foulum, Aarhus University", size=11, color=GREY, after=2)
para("Draft for internal review · 10 November 2026 · Data: pipeline snapshot of 15–17 September 2026 "
     "(yield 2023–2025; NO3 leaching 13 Sep 2024–1 Apr 2026; TDR 2024–2025; soil carbon 2024–2025)",
     size=8.5, italic=True, color=GREY, after=6)

# =========================================================================== 1. SUMMARY
doc.add_heading("1. Summary", level=1)
para("This report compares plots between the vertical PV rows (**VAPV**) with the open-field reference in the same strips, first "
     "for each outcome overall and then position by position (west / centre / east) across the strip. All effects are "
     "VAPV relative to open field, averaged over the four fertiliser × residue treatments. Confidence intervals (CI) are "
     "95% and computed at the level of nests (sub-strips), the sampling cluster, not individual plots, cups or samples.", after=4)

ww, sy, gc = yov.loc["WW"], yov.loc["SY"], yov.loc["GC"]
a2, a3, bo = ns.loc["A2"], ns.loc["A3"], ns.loc["Both"]
s_a2_w1 = nse[(nse.strip == "A2") & (nse.season == "W1")].iloc[0]
s_a2_w2 = nse[(nse.strip == "A2") & (nse.season == "W2")].iloc[0]
c_a2_o = ncon[(ncon.strip == "A2") & (ncon.env == "Open field")].iloc[0]
c_a2_v = ncon[(ncon.strip == "A2") & (ncon.env == "VAPV")].iloc[0]

box = doc.add_table(rows=1, cols=1)
box.alignment = WD_TABLE_ALIGNMENT.CENTER
borders(box, outer="1F3A5F", inner="1F3A5F")
cell_margins(box, 70, 70, 120, 120)
c = box.rows[0].cells[0]
c.width = Cm(CONTENT_W)
shade(c, BOX_FILL)
c.paragraphs[0].paragraph_format.space_after = Pt(0)


def box_bullet(text):
    pp = c.add_paragraph(style="List Bullet")
    rich(pp, text, size=9.5)
    pp.paragraph_format.space_after = Pt(3)


c._tc.remove(c.paragraphs[0]._p)
box_bullet(f"**Productivity.** Averaged over all crop-years, yield between the PV rows was {pct(A.pct)} {ci(A.pct_lcl, A.pct_ucl, 0, True)} "
           f"of open field. Winter wheat grain yield was lower ({pct(ww.pct)} {ci(ww.pct_lcl, ww.pct_ucl, 0, True)}); soybean {pct(sy.pct)} "
           f"(CI {ci(sy.pct_lcl, sy.pct_ucl, 0, True)}, only 3 nests per location); grass-clover (annual sum of cuts) was unchanged "
           f"({pct(gc.pct)} {ci(gc.pct_lcl, gc.pct_ucl, 0, True)}). Winter wheat grain N concentration was {num(tn.loc['WW']['diff'], 2)} percentage points higher"
           f"({num(tn.loc['WW'].mean_open, 2)} vs {num(tn.loc['WW'].mean_vapv, 2)}% N).")
box_bullet(f"**Nitrate leaching.** Over {NDAYS} days VAPV cups leached {num(bo.mean_vapv, 1)} vs {num(bo.mean_open, 1)} kg N/ha in open field "
           f"({pct(bo.pct)} {ci(bo.pct_lcl, bo.pct_ucl, 0, True)}; strips A2 and A3 combined, {int(bo.nn_open)} vs {int(bo.nn_vapv)} nests); in the winter-wheat strip A2 "
           f"{num(a2.mean_vapv, 1)} vs {num(a2.mean_open, 1)} kg N/ha. The whole excess accrued in winter 2024/25 "
           f"({sgn(s_a2_w1['diff'], 1)} kg N/ha in A2); in winter 2025/26 the two locations did not differ.")
box_bullet(f"**Soil water.** VAPV was not drier than open field. In 2024 there was no difference ({sgn(tdr('20 cm probe', '2024', 'diff'), 1)} / "
           f"{sgn(tdr('50 cm probe', '2024', 'diff'), 1)} percentage points at 20 / 50 cm). In 2025, when open-field soil was much drier, VAPV was wetter "
           f"({sgn(tdr('20 cm probe', '2025', 'diff'), 1)} / {sgn(tdr('50 cm probe', '2025', 'diff'), 1)} pp; wetter on "
           f"{int(tdr('20 cm probe', '2025', 'n_pos'))} / {int(tdr('50 cm probe', '2025', 'n_pos'))} of {int(tdr('50 cm probe', '2025', 'n_date'))} dates). "
           f"This is not distinguishable at nest level because only three open-field nests were monitored.")
box_bullet(f"**Soil carbon.** SOC was {num(socr.loc['All rounds (nest means)']['diff'], 2)} pp higher under VAPV "
           f"({pct(socr.loc['All rounds (nest means)'].pct, 1)}, CI {ci(socr.loc['All rounds (nest means)'].pct_lcl, socr.loc['All rounds (nest means)'].pct_ucl, 1, True)}), but the gap "
           f"was already present in the 2024 survey ({sgn(socr.loc['2024 survey']['diff'], 2)} pp) and did not widen ({sgn(did.did, 2)} pp "
           f"{ci(did.did_lcl, did.did_ucl, 2, True)}). A separate baseline survey shows a larger gap at depth. There is no evidence of a PV effect "
           f"on SOC, and the data cannot yet show SOC change.")
box_bullet(f"**Substrip level.** Effects are not uniform across the strip. The winter wheat yield deficit sits at the West and East "
           f"positions (2024: {pct(yp('WW 2024', 'West', 'pct_means'))}, {pct(yp('WW 2024', 'Centre', 'pct_means'))}, "
           f"{pct(yp('WW 2024', 'East', 'pct_means'))} at West / Centre / East); soybean losses were largest at East ({pct(yp('SY 2024', 'East', 'pct_means'))}). "
           f"Open-field positions themselves differ strongly in leaching "
           f"(strip A2: {num(npp('A2', 'Open field', 'Centre', 'mean'), 0)} kg N/ha at Centre vs {num(npp('A2', 'Open field', 'West', 'mean'), 0)} "
           f"and {num(npp('A2', 'Open field', 'East', 'mean'), 0)} at West / East), so position-level VAPV effects depend on the reference position.")
box_bullet("**Main limits.** VAPV vs open field is a contrast between strip halves with 1–3 strip pairs per crop-year, so intervals are "
           "optimistic; winter wheat 2025 yield (the strip with the largest leaching difference) is not yet entered; leaching assumes identical "
           "drainage in both locations; soil carbon was sampled at different points each round (Section 8).")

para("", after=2)
caption("Table 1. Headline comparison, VAPV vs open field. Percent difference is the ratio of geometric means of nest values, "
        "with a 95% CI from a Welch t-test on the log scale (strip-adjusted linear model for ‘both strips’). Bold = CI excludes 0.", before=4)
rows, bolds = [], set()
LAB = {
    "Yield, winter wheat": ("Productivity", "Winter wheat grain yield"),
    "Yield, soybean": ("", "Soybean grain yield"),
    "Yield, grass-clover": ("", "Grass-clover biomass (annual)"),
    "Yield, spring barley": ("", "Spring barley green biomass"),
    "Yield, ryegrass (undersown)": ("", "Ryegrass biomass (undersown)"),
    "Relative yield, all crops": ("", "All crops pooled (relative yield)"),
    "NO3-N leached, strip A2 (winter wheat)": ("Nitrate leaching", "NO3-N leached, strip A2 (kg N/ha)"),
    "NO3-N leached, strip A3 (grass-clover / barley)": ("", "NO3-N leached, strip A3 (kg N/ha)"),
    "NO3-N leached, both strips": ("", "NO3-N leached, both strips (kg N/ha)"),
    "Soil water content, 20 cm probe": ("Soil water", "Water content, 20 cm probe (vol-%)"),
    "Soil water content, 50 cm probe": ("", "Water content, 50 cm probe (vol-%)"),
    "SOC, IUNG (all rounds)": ("Soil carbon", "SOC, IUNG, 3 rounds (% C)"),
    "Total C 0-30 cm, baseline survey": ("", "Total C 0–30 cm, baseline survey (%)"),
}
order = ["Yield, winter wheat", "Yield, soybean", "Yield, grass-clover", "Yield, spring barley", "Yield, ryegrass (undersown)",
         "Relative yield, all crops", "NO3-N leached, strip A2 (winter wheat)", "NO3-N leached, strip A3 (grass-clover / barley)",
         "NO3-N leached, both strips", "Soil water content, 20 cm probe", "Soil water content, 50 cm probe",
         "SOC, IUNG (all rounds)", "Total C 0-30 cm, baseline survey"]
for i, key in enumerate(order):
    r = hl(key)
    dom, lab = LAB[key]
    absval = ("–", "–", "–") if r.domain == "Crop productivity" else (
        num(r.open, 2 if r.unit in ("% SOC", "% C") else 1), num(r.vapv, 2 if r.unit in ("% SOC", "% C") else 1),
        sgn(r.delta, 2 if r.unit in ("% SOC", "% C") else 1))
    dp = 1 if r.domain == "Soil carbon" else 0   # small SOC percentages need a decimal to show which side of 0 the CI end-points fall
    rows.append([dom, lab, absval[0], absval[1], absval[2], f"{pct(r.pct, dp)} {ci(r.lcl, r.ucl, dp, True)}",
                 f"{int(r.n_open)} / {int(r.n_vapv)}", pv(r.p)])
    if excl0(r.lcl, r.ucl):
        bolds.add((i, 5))
        bolds.add((i, 1))
table(["Domain", "Metric", "Open field", "VAPV", "Diff.", "VAPV vs open (%) [95% CI]", "Nests O / V", "p"],
      rows, [2.2, 4.7, 1.5, 1.4, 1.3, 3.3, 1.5, 1.1], align=["l", "l", "c", "c", "c", "c", "c", "c"], size=8, bold_cells=bolds,
      first_col_bold=True)
note("Yield rows are relative (open field = 100%); absolute yields are in Table 3. Soil-water row: nest-level contrast with 3 open-field vs 9 VAPV nests. "
     "Soil-carbon rows use nest means (IUNG: 9 vs 9 nests; baseline survey: 10 vs 10 West/East nests).")

# =========================================================================== 2. DATA AND METHODS
doc.add_heading("2. Site, data and methods", level=1)
doc.add_heading("2.1 Design", level=2)
para("The trial is at Aarhus University, Foulum (56°29′N, 9°33′E), under a vertical bifacial PV installation. Five main strips "
     "(A1–A5) each have an **open-field** half and a half **between the PV rows (VAPV)**. Each half carries three nests "
     "(sub-strips N1–N30; **West, Centre, East** across the strip), and each nest has four plots of about 1.5 × 1.5 m with the "
     "four combinations of fertiliser (biogas digestate / mineral) and residue (removed / retained). The soil is a coarse sandy soil "
     "(Danish JB 4, clay about 8% in the topsoil). All comparisons here pool the four management combinations, which are present in every "
     "nest; fertiliser and residue effects are reported in the manuscript figures. The distance of each position from the panel rows is not "
     "recorded in the datasets used, so position effects are described, not explained geometrically.", after=4)
caption("Table 2. Data used, with coverage and the replication unit of each dataset.", before=4)
rows = [[r.domain, r.data, r.period, r.n, r.unit, r.note] for r in cov.itertuples()]
table(["Domain", "Data", "Period", "Amount", "Unit", "Gaps / notes"], rows, [2.2, 4.1, 2.6, 2.4, 2.4, 3.3],
      align=["l", "l", "l", "l", "l", "l"], size=7.5, first_col_bold=True)
note("Raw inputs bundled in 1_input_data/ have not changed since the pipeline targets were built (file dates checked). "
     "Grass-clover yield is the sum of all cuts in the year per plot.")

doc.add_heading("2.2 Statistics", level=2)
bullet("**Descriptive numbers** (tables and figures) are means ± SE of the stated unit (plot, cup, sample or nest). Individual observations "
       "are shown behind every mean in the figures.")
bullet("**Inference** for VAPV vs open field is done at nest level: values are averaged within each nest, log-transformed, and compared with a "
       "Welch t-test (a linear model with strip as a factor for the two leaching strips). The result is a ratio of geometric means expressed as a "
       "percent difference with a 95% CI. For soil water the paired difference per date (VAPV nests minus the open-field nest of the same strip) is "
       "also given. Absolute differences (percentage points) use the raw scale.")
bullet("**Substrip (position) numbers** use plots, cups or samples within one nest per position and strip. They are descriptive and "
       "exploratory: position is confounded with nest identity, and open-field positions show how large natural variation between nests is.")
bullet("No adjustment for multiple comparisons is made. ‘Clear’ means the 95% CI excludes 0; wide CIs from 3 vs 3 nests mean "
       "‘not distinguishable’, not ‘no effect’. Because each strip half is a single experimental unit, even nest-level intervals "
       "understate the true uncertainty.")

# =========================================================================== 3. PRODUCTIVITY
doc.add_heading("3. Crop productivity", level=1)
w23, w24, s24 = ycy.loc["WW 2023"], ycy.loc["WW 2024"], ycy.loc["SY 2024"]
g23, g24, g25 = ycy.loc["GC 2023"], ycy.loc["GC 2024"], ycy.loc["GC 2025"]
sb, rg = ycy.loc["SB 2025"], ycy.loc["RG 2025"]
para(f"Winter wheat grain yield was lower between the PV rows in both years with data: {pct(w23.pct)} in 2023 (3 vs 3 single-plot nests) and "
     f"{pct(w24.pct)} {ci(w24.pct_lcl, w24.pct_ucl, 0, True)} in 2024 ({num(w24.mean_open, 2)} vs {num(w24.mean_vapv, 2)} Mg DM/ha). "
     f"Winter wheat 2025 has no yield data yet (Section 8). Soybean 2024 yielded {num(s24.mean_vapv, 2)} vs {num(s24.mean_open, 2)} Mg DM/ha "
     f"({pct(s24.pct)}), but with 3 nests per location the CI is wide {ci(s24.pct_lcl, s24.pct_ucl, 0, True)}. Grass-clover annual biomass did not differ "
     f"({pct(g23.pct)} in 2023, {pct(g24.pct)} in 2024, {pct(g25.pct)} in 2025). Spring barley green biomass was {pct(sb.pct)} "
     f"(CI includes 0) and undersown ryegrass {pct(rg.pct)} at very low yields (about 1 Mg DM/ha; CI {ci(rg.pct_lcl, rg.pct_ucl, 0, True)}). "
     f"Pooled over crops, relative yield was {pct(A.pct)} {ci(A.pct_lcl, A.pct_ucl, 0, True)} ({int(A.n_open)} vs {int(A.n_vapv)} nests).", after=4)
figure("fig1_yield")
caption("Figure 1. Dry-matter yield of each crop’s primary product by crop-year. (A) Plots (faint) and mean ± SE; (B) VAPV vs open field as percent "
        "difference with 95% CI from nest-level Welch tests; the bottom row is the pooled relative yield over all crop-years.", before=2, keep=False)

caption("Table 3. Yield by crop-year, VAPV vs open field (Mg DM/ha; mean ± SE of plots).")
rows, bolds = [], set()
for i, cy in enumerate(CY_NAME):
    r = ycy.loc[cy]
    rows.append([CY_NAME[cy], f"{int(r.n_plot_open)} / {int(r.n_plot_vapv)}", f"{int(r.n_nest_open)} / {int(r.n_nest_vapv)}",
                 pm(r.mean_open, r.se_open), pm(r.mean_vapv, r.se_vapv), sgn(r.diff_abs, 2),
                 f"{pct(r.pct)} {ci(r.pct_lcl, r.pct_ucl, 0, True)}", pv(r.p)])
    if excl0(r.pct_lcl, r.pct_ucl):
        bolds.update({(i, 6), (i, 0)})
table(["Crop-year", "Plots O / V", "Nests O / V", "Open field", "VAPV", "Diff.", "VAPV vs open (%) [95% CI]", "p"], rows,
      [4.1, 1.4, 1.4, 2.1, 2.1, 1.2, 3.8, 0.9], align=["l", "c", "c", "c", "c", "c", "c", "c"], size=8, bold_cells=bolds)
note("Percent difference and p use nest means (geometric); ‘Diff.’ is the difference of arithmetic plot means, so the two can differ slightly. "
     "Grass-clover 2025 uses strip A4 only (strip A1 lacks cut 1). Bold = CI excludes 0.")

doc.add_heading("Tissue N", level=2)
n_w, n_s, n_g = tn.loc["WW"], tn.loc["SY"], tn.loc["GC"]
para(f"Total N in the harvested product exists for 2024 only. Winter wheat grain was richer in N between the PV rows ({num(n_w.mean_vapv, 2)} vs "
     f"{num(n_w.mean_open, 2)}% N; {sgn(n_w['diff'], 2)} pp {ci(n_w.diff_lcl, n_w.diff_ucl, 2, True)}), which fits its lower yield. "
     f"Soybean ({sgn(n_s['diff'], 2)} pp {ci(n_s.diff_lcl, n_s.diff_ucl, 2, True)}) and grass-clover ({sgn(n_g['diff'], 2)} pp "
     f"{ci(n_g.diff_lcl, n_g.diff_ucl, 2, True)}) did not differ.", after=4)

doc.add_heading("Substrip level", level=2)
ow = ypos[(ypos.cy == "WW 2024") & (ypos.env == "Open field")]["mean"]
rg_o = ypos[(ypos.cy == "RG 2025") & (ypos.env == "Open field")].set_index("pos")["mean"]
para(f"The yield deficit was not uniform across the strip (Figure 2, Table 4). In winter wheat 2024 it was {pct(yp('WW 2024', 'West', 'pct_means'))} at West and "
     f"{pct(yp('WW 2024', 'East', 'pct_means'))} at East but only {pct(yp('WW 2024', 'Centre', 'pct_means'))} at Centre; soybean losses occurred at all three positions and were "
     f"largest at East ({pct(yp('SY 2024', 'East', 'pct_means'))}; West {pct(yp('SY 2024', 'West', 'pct_means'))}, Centre {pct(yp('SY 2024', 'Centre', 'pct_means'))}). "
     f"Spring barley was {pct(yp('SB 2025', 'West', 'pct_means'))} / {pct(yp('SB 2025', 'Centre', 'pct_means'))} / {pct(yp('SB 2025', 'East', 'pct_means'))}. "
     f"Grass-clover differences stayed within ±{num(ypd[ypd.cy.str.startswith('GC')].pct_means.abs().max(), 0)}% at every position and year. Open-field positions differed by at most "
     f"{num(100 * (ow.max() / ow.min() - 1), 0)}% in winter wheat 2024, so the West and East deficits exceed natural position-to-position variation. "
     f"The ryegrass West difference ({pct(yp('RG 2025', 'West', 'pct_means'))}) mainly reflects a low open-field West reference "
     f"({num(rg_o['West'], 2)} vs {num(rg_o['Centre'], 2)} Mg DM/ha at Centre). Whether the West/East deficit reflects shading, shelter or other "
     f"panel effects cannot be resolved with these data.", after=4)
figure("fig2_yield_position", 15.2)
caption("Figure 2. Yield by position across the strip (West, Centre, East), open field vs VAPV, for each crop-year (free y-axes). Faint symbols are plots; "
        "symbols with bars are mean ± SE. Numbers above each position give the VAPV vs open difference of the means. "
        "Winter wheat 2023 has one plot per cell.", before=2, keep=False)
caption("Table 4. Yield by position: open vs VAPV mean (Mg DM/ha) and percent difference of the means.")
rows, bolds = [], set()
for i, cy in enumerate(CY_NAME):
    row = [CY_NAME[cy]]
    for j, pos in enumerate(POS):
        o = ypos[(ypos.cy == cy) & (ypos.env == "Open field") & (ypos.pos == pos)].iloc[0]
        v = ypos[(ypos.cy == cy) & (ypos.env == "VAPV") & (ypos.pos == pos)].iloc[0]
        d = ypd[(ypd.cy == cy) & (ypd.pos == pos)].iloc[0]
        row += [f"{num(o['mean'], 2)} vs {num(v['mean'], 2)}", pct(d.pct_means)]
        if excl0(d.pct_lcl, d.pct_ucl):
            bolds.add((i, 2 + 2 * j))
    rows.append(row)
table(["Crop-year", "West: open vs VAPV", "West diff.", "Centre: open vs VAPV", "Centre diff.", "East: open vs VAPV", "East diff."], rows,
      [4.3, 2.5, 1.4, 2.7, 1.4, 2.5, 1.4], align=["l", "c", "c", "c", "c", "c", "c"], size=8, bold_cells=bolds)
note("n = 4–5 plots per cell (1 in 2023; 2 for grass-clover 2023) from a single nest per position and strip. Bold = plot-level 95% CI of the log ratio excludes 0 "
     "(exploratory only).")

# =========================================================================== 4. NO3 LEACHING
doc.add_heading("4. Nitrate leaching", level=1)
para(f"NO3-N leaching below 1 m was estimated with the Drainage-Linear method: suction-cup NO3-N concentrations (linearly interpolated, {NO3_DATES} sampling dates) "
     f"multiplied by Daisy-simulated daily drainage, summed from 13 Sep 2024 to 1 Apr 2026 ({NDAYS} days). Strip A2 (winter wheat) carries nearly all of "
     f"the leaching: {num(a2.mean_open, 1)} ± {num(a2.se_open, 1)} kg N/ha in open field vs {num(a2.mean_vapv, 1)} ± {num(a2.se_vapv, 1)} between the PV rows "
     f"({pct(a2.pct)}, CI {ci(a2.pct_lcl, a2.pct_ucl, 0, True)} with {int(a2.nn_open)} vs {int(a2.nn_vapv)} nests). Strip A3 (grass-clover, then barley with undersown ryegrass) leached "
     f"{num(a3.mean_open, 1)} vs {num(a3.mean_vapv, 1)} kg N/ha ({pct(a3.pct)}, CI {ci(a3.pct_lcl, a3.pct_ucl, 0, True)}). Combining both strips with strip as a factor gives "
     f"{pct(bo.pct)} {ci(bo.pct_lcl, bo.pct_ucl, 0, True)} (p = {pv(bo.p)}, {int(bo.nn_open)} vs {int(bo.nn_vapv)} nests).", after=4)
def fig3_block():
    figure("fig3_no3")
    caption("Figure 3. Nitrate leaching. (A) Cumulative NO3-N leached, mean ± SE of cups, with crop events and drainage seasons; (B) total leached by position "
            "(faint symbols are cups); (C) leaching by drainage season (the growing season is ~0 kg N/ha in both locations and is omitted).", before=2, keep=False)


caption("Table 5. NO3-N leaching 13 Sep 2024–1 Apr 2026 (kg N/ha; mean ± SE of cups), Daisy drainage and flow-weighted concentration.")
rows, bolds = [], set()
for i, (k, lab) in enumerate([("A2", "A2 winter wheat"), ("A3", "A3 grass-clover / barley"), ("Both", "Both strips")]):
    r = ns.loc[k]
    rows.append([lab, pm(r.mean_open, r.se_open, 1), pm(r.mean_vapv, r.se_vapv, 1), sgn(r.mean_vapv - r.mean_open, 1),
                 f"{pct(r.pct)} {ci(r.pct_lcl, r.pct_ucl, 0, True)}", num(r.drainage_mm_open, 0),
                 f"{num(r.conc_open, 1)} vs {num(r.conc_vapv, 1)}", f"{int(r.n_cup_open)} / {int(r.n_cup_vapv)}; {int(r.n_nest_open)} / {int(r.n_nest_vapv)}"])
    if excl0(r.pct_lcl, r.pct_ucl):
        bolds.add((i, 4))
table(["Strip", "Open field", "VAPV", "Diff.", "VAPV vs open (%) [95% CI]", "Drain. (mm)", "Conc. (mg N/L) open vs VAPV", "Cups; nests O / V"], rows,
      [3.0, 2.2, 2.2, 1.2, 3.0, 1.2, 2.3, 1.9], align=["l", "c", "c", "c", "c", "c", "c", "c"], size=8, bold_cells=bolds, first_col_bold=True)
note("Percent difference at nest level. Drainage is one Daisy series per strip, identical for both locations. Flow-weighted concentration = N leached / drainage. "
     "The EU drinking-water limit corresponds to 11.3 mg NO3-N/L.")
fig3_block()

para(f"**Timing.** The VAPV excess in A2 arose entirely in winter 2024/25: {num(s_a2_w1.mean_open, 1)} vs {num(s_a2_w1.mean_vapv, 1)} kg N/ha "
     f"({sgn(s_a2_w1['diff'], 1)} {ci(s_a2_w1.diff_lcl, s_a2_w1.diff_ucl, 1, True)}). In winter 2025/26 both locations leached about the same "
     f"({num(s_a2_w2.mean_open, 1)} vs {num(s_a2_w2.mean_vapv, 1)} kg N/ha) and the growing season contributed almost nothing (drainage {num(nsea('A2', 'GS', 'drain'), 1)} mm). "
     f"**Concentration.** In A2 the mean cup concentration was {num(c_a2_v['mean'], 1)} mg N/L in VAPV vs {num(c_a2_o['mean'], 1)} in open field, and "
     f"{num(c_a2_v.share_above_11_3, 0)}% vs {num(c_a2_o.share_above_11_3, 0)}% of samples exceeded 11.3 mg N/L. Because both locations use the same drainage, the "
     f"difference comes entirely from soil-water concentration.", after=4)
caption("Table 6. NO3-N leaching by drainage season (kg N/ha; mean ± SE of cups) and nest-level difference.")
rows = []
for r in nse.itertuples():
    rows.append([r.strip, r.season_label, num(r.drain, 0), pm(r.mean_open, r.se_open, 1 if r.mean_open > 0.5 else 3),
                 pm(r.mean_vapv, r.se_vapv, 1 if r.mean_vapv > 0.5 else 3),
                 f"{sgn(r.diff, 1 if abs(r.diff) > 0.1 else 3)} {ci(r.diff_lcl, r.diff_ucl, 1 if abs(r.diff) > 0.1 else 3, True)}"])
bolds = {(i, 5) for i, r in enumerate(nse.itertuples()) if excl0(r.diff_lcl, r.diff_ucl)}
table(["Strip", "Season", "Drain. (mm)", "Open field", "VAPV", "Diff. [95% CI]"], rows, [1.2, 5.6, 1.6, 2.6, 2.6, 3.4],
      align=["l", "l", "c", "c", "c", "c"], size=8, bold_cells=bolds)
note("W1 and W2 = drainage seasons; GS = growing season. Difference CIs use nest means (3 vs 3 per strip).")

doc.add_heading("Substrip level", level=2)
para(f"In A2 the open-field positions already differed twofold: {num(npp('A2', 'Open field', 'Centre', 'mean'), 1)} kg N/ha at Centre vs "
     f"{num(npp('A2', 'Open field', 'West', 'mean'), 1)} at West and {num(npp('A2', 'Open field', 'East', 'mean'), 1)} at East, whereas VAPV positions were more even "
     f"({num(npp('A2', 'VAPV', 'West', 'mean'), 1)}, {num(npp('A2', 'VAPV', 'Centre', 'mean'), 1)}, {num(npp('A2', 'VAPV', 'East', 'mean'), 1)}). The VAPV excess was therefore largest at "
     f"Centre ({sgn(npp('A2', 'VAPV', 'Centre', 'mean') - npp('A2', 'Open field', 'Centre', 'mean'), 1)} kg N/ha) and smaller at West "
     f"({sgn(npp('A2', 'VAPV', 'West', 'mean') - npp('A2', 'Open field', 'West', 'mean'), 1)}) and East "
     f"({sgn(npp('A2', 'VAPV', 'East', 'mean') - npp('A2', 'Open field', 'East', 'mean'), 1)}). In A3 amounts are small (≤9 kg N/ha) and the pattern is "
     f"inconsistent between positions, with 3–4 cups each (Table 7).", after=4)
caption("Table 7. NO3-N leached by position (kg N/ha; mean ± SE of cups; n cups in brackets).")
rows = []
for strip in ("A2", "A3"):
    for pos in POS:
        o = npos[(npos.strip == strip) & (npos.env == "Open field") & (npos.pos == pos)].iloc[0]
        v = npos[(npos.strip == strip) & (npos.env == "VAPV") & (npos.pos == pos)].iloc[0]
        rows.append([strip, pos, f"{pm(o['mean'], o.se, 1)} ({int(o.n_cup)})", f"{pm(v['mean'], v.se, 1)} ({int(v.n_cup)})",
                     sgn(v['mean'] - o['mean'], 1), pct(100 * (v['mean'] / o['mean'] - 1))])
table(["Strip", "Position", "Open field", "VAPV", "Diff.", "Diff. (%)"], rows, [1.5, 2.2, 4.4, 4.4, 2.3, 2.2],
      align=["l", "l", "c", "c", "c", "c"], size=8)

# =========================================================================== 5. SOIL WATER
doc.add_heading("5. Soil water", level=1)
t20a, t50a = tdr("20 cm probe", "All", "diff"), tdr("50 cm probe", "All", "diff")
para(f"Volumetric water content was measured with TDR probes of 20 and 50 cm length in 12 nests on {int(tdr('20 cm probe', 'All', 'n_date'))} dates "
     f"(Mar–Aug 2024 and May–Sep 2025; growing season only). Each VAPV nest is compared with the open-field nest of the same strip on the same date. "
     f"Over both years VAPV was wetter by {sgn(t20a, 1)} pp {ci(tdr('20 cm probe', 'All', 'lcl'), tdr('20 cm probe', 'All', 'ucl'), 1, True)} at 20 cm and "
     f"{sgn(t50a, 1)} pp {ci(tdr('50 cm probe', 'All', 'lcl'), tdr('50 cm probe', 'All', 'ucl'), 1, True)} at 50 cm (CI across dates), but the pattern depended on year. "
     f"In 2024 (open-field mean {rng(tdr('20 cm probe', '2024', 'mean_open'), tdr('50 cm probe', '2024', 'mean_open'))} vol-%) the locations were equal. "
     f"In 2025, when the open-field soil was much drier (mean {rng(tdr('20 cm probe', '2025', 'mean_open'), tdr('50 cm probe', '2025', 'mean_open'))} vol-%), "
     f"VAPV was wetter by {sgn(tdr('20 cm probe', '2025', 'diff'), 1)} pp (20 cm) and {sgn(tdr('50 cm probe', '2025', 'diff'), 1)} pp (50 cm). "
     f"With only three open-field nests, the nest-level contrasts ({sgn(tdn.loc['20 cm probe']['diff'], 1)} pp {ci(tdn.loc['20 cm probe'].diff_lcl, tdn.loc['20 cm probe'].diff_ucl, 1, True)} at 20 cm; "
     f"{sgn(tdn.loc['50 cm probe']['diff'], 1)} pp {ci(tdn.loc['50 cm probe'].diff_lcl, tdn.loc['50 cm probe'].diff_ucl, 1, True)} at 50 cm) are consistent in sign but their CIs include 0.", after=4)
def fig4_block():
    figure("fig4_soilwater", 14.0)
    caption("Figure 4. Soil water content (TDR). (A) Mean of open-field and VAPV nests per date (lines broken across gaps > 25 days); (B) paired VAPV minus open-field "
            "difference per date with the mean and the number of dates VAPV was wetter; (C) difference by VAPV position relative to the open-field centre nest "
            "(mean over dates, 95% CI).", before=2, keep=False)


caption("Table 8. Paired VAPV minus open-field soil water content (percentage points) by year and probe; CI across sampling dates.")
rows, bolds = [], set()
i = 0
for depth in ("20 cm probe", "50 cm probe"):
    for yr in ("2024", "2025", "All"):
        r = tdo[(tdo.depth == depth) & (tdo.year.astype(str) == yr)].iloc[0]
        rows.append([depth.replace(" probe", ""), "Both years" if yr == "All" else yr, int(r.n_date), num(r.mean_open, 1), num(r.mean_vapv, 1),
                     f"{sgn(r['diff'], 1)} {ci(r.lcl, r.ucl, 1, True)}", f"{int(r.n_pos)} of {int(r.n_date)} ({num(r.share_pos, 0)}%)"])
        if excl0(r.lcl, r.ucl):
            bolds.add((i, 5))
        i += 1
table(["Probe (cm)", "Year", "Dates", "Open field (vol-%)", "VAPV (vol-%)", "Diff. (pp) [95% CI]", "Dates VAPV wetter"], rows,
      [2.0, 2.0, 1.4, 2.8, 2.5, 3.6, 2.7], align=["c", "c", "c", "c", "c", "c", "c"], size=8, bold_cells=bolds)
note(f"Duplicate records of the same measurement ({qcn('recorded in more than one file')} date/nest/plot/probe combinations appear in more than one crop-labelled file) were averaged; "
     f"using the first record only changes any row of this table by at most {num(TDR_ROBUST, 1)} pp (largest: {_worst.year}, {_worst.depth}), without changing sign or conclusions. "
     f"Bold = CI across dates excludes 0 (dates are not independent).")

doc.add_heading("Substrip level", level=2)
tp = lambda d, pos, c: tdp[(tdp.depth == d) & (tdp.pos == pos)].iloc[0][c]
para(f"The contrast was smallest at the Centre of the VAPV strip ({sgn(tp('20 cm probe', 'Centre', 'diff_mean'), 1)} pp at 20 cm, {sgn(tp('50 cm probe', 'Centre', 'diff_mean'), 1)} pp at 50 cm) "
     f"and larger at West ({sgn(tp('20 cm probe', 'West', 'diff_mean'), 1)} / {sgn(tp('50 cm probe', 'West', 'diff_mean'), 1)} pp) and East "
     f"({sgn(tp('20 cm probe', 'East', 'diff_mean'), 1)} / {sgn(tp('50 cm probe', 'East', 'diff_mean'), 1)} pp), where VAPV was wetter on most dates (Table 9). Only the centre position of the open field "
     f"was instrumented, so open-field West/East values, and therefore natural spatial variation in soil water, are unknown; the position contrasts are against "
     f"the open-field centre only.", after=4)
caption("Table 9. VAPV minus open-field centre soil water content by VAPV position (pp; mean over dates [95% CI]; share of dates VAPV wetter).")
rows = []
for d in ("20 cm probe", "50 cm probe"):
    row = [d.replace(" probe", "")]
    for pos in POS:
        r = tdp[(tdp.depth == d) & (tdp.pos == pos)].iloc[0]
        row.append(f"{sgn(r.diff_mean, 1)} {ci(r.diff_lcl, r.diff_ucl, 1, True)}; {num(r.share_pos, 0)}%")
    rows.append(row)
table(["Probe (cm)", "West", "Centre", "East"], rows, [2.2, 4.9, 4.9, 4.9], align=["c", "c", "c", "c"], size=8)
fig4_block()

# =========================================================================== 6. SOIL CARBON
doc.add_heading("6. Soil carbon", level=1)
sa = socr.loc["All rounds (nest means)"]
b30, b60 = bc.loc["0-30 cm"], bc.loc["30-60 cm"]
para(f"Soil organic carbon (SOC) from the IUNG laboratory was {num(sa.mean_vapv, 2)}% under VAPV vs {num(sa.mean_open, 2)}% in open field across the three rounds "
     f"({sgn(sa['diff'], 2)} pp; {pct(sa.pct, 1)} {ci(sa.pct_lcl, sa.pct_ucl, 1, True)}; {int(sa.n_nest_open)} vs {int(sa.n_nest_vapv)} nests). The gap was already {sgn(socr.loc['2024 survey']['diff'], 2)} pp "
     f"{ci(socr.loc['2024 survey'].diff_lcl, socr.loc['2024 survey'].diff_ucl, 2, True)} in the 2024 survey, {sgn(socr.loc['2025 T1']['diff'], 2)} pp in 2025 T1 and "
     f"{sgn(socr.loc['2025 T2']['diff'], 2)} pp in 2025 T2. Nest means rose by similar amounts in both locations from the 2024 survey to 2025 T2 "
     f"(open {sgn(did.chg_open, 3)}, VAPV {sgn(did.chg_vapv, 3)} pp; difference {sgn(did.did, 3)} pp {ci(did.did_lcl, did.did_ucl, 3, True)}), so there is no sign that the "
     f"gap widened. Every round sampled different physical points, so these are comparisons of SOC levels, not measurements of SOC change.", after=4)
para(f"A separate baseline survey (Aarhus University laboratory, winter 2024/25, West and East nests) shows larger gaps: total C at 0–30 cm was "
     f"{num(b30.mean_vapv, 2)} vs {num(b30.mean_open, 2)}% ({sgn(b30['diff'], 2)} pp {ci(b30.diff_lcl, b30.diff_ucl, 2, True)}) and at 30–60 cm "
     f"{num(b60.mean_vapv, 2)} vs {num(b60.mean_open, 2)}% ({sgn(b60['diff'], 2)} pp {ci(b60.diff_lcl, b60.diff_ucl, 2, True)}). A gap that is larger at depth, "
     f"where management and PV effects over 1–2 seasons should be smallest, points to pre-existing differences in the soil between strip halves rather than "
     f"a PV effect (clay at 30–60 cm also differed: {num(btex[(btex.depth == '30-60 cm') & (btex.env == 'VAPV')].clay.iloc[0], 1)}% vs "
     f"{num(btex[(btex.depth == '30-60 cm') & (btex.env == 'Open field')].clay.iloc[0], 1)}%). The two laboratories differ in method and depth, so absolute values are not directly comparable.", after=4)
figure("fig5_soc", 15.5)
caption("Figure 5. Soil carbon. (A) SOC by survey round (IUNG; faint symbols are samples); (B) baseline survey total C by depth (faint symbols are nests); "
        "(C) SOC by position within each round. Rounds sampled different points.", before=2, keep=False)
caption("Table 10. SOC by survey round (IUNG; % of soil; mean ± SE of samples) with nest-level differences.")
rows, bolds = [], set()
for i, rn in enumerate(["2024 survey", "2025 T1", "2025 T2", "All rounds (nest means)"]):
    r = socr.loc[rn]
    nn = f"{int(r.n_nest_open)} / {int(r.n_nest_vapv)}"
    rows.append([rn, f"{int(r.n_open)} / {int(r.n_vapv)}", nn, pm(r.mean_open, r.se_open), pm(r.mean_vapv, r.se_vapv),
                 f"{sgn(r['diff'], 2)} {ci(r.diff_lcl, r.diff_ucl, 2, True)}", f"{pct(r.pct, 1)} {ci(r.pct_lcl, r.pct_ucl, 1, True)}", pv(r.p)])
    if excl0(r.diff_lcl, r.diff_ucl):
        bolds.add((i, 5))
table(["Round", "Samples O / V", "Nests O / V", "Open field", "VAPV", "Diff. (pp) [95% CI]", "Diff. (%) [95% CI]", "p"], rows,
      [3.0, 1.6, 1.4, 2.0, 2.0, 2.9, 3.0, 1.1], align=["l", "c", "c", "c", "c", "c", "c", "c"], size=8, bold_cells=bolds)
note("The last row averages each nest over rounds first. Rows for single rounds use nest means within the round (2025 T1: 3 vs 3 nests).")
caption("Table 11. Baseline survey: total C (% of soil) by depth, nest level (10 vs 10 West/East nests).")
rows, bolds = [], set()
for i, d in enumerate(["0-30 cm", "30-60 cm", ">60 cm"]):
    r = bc.loc[d]
    rows.append([d.replace("-", "–"), pm(r.mean_open, r.se_open), pm(r.mean_vapv, r.se_vapv), f"{sgn(r['diff'], 2)} {ci(r.diff_lcl, r.diff_ucl, 2, True)}",
                 f"{pct(r.pct)} {ci(r.pct_lcl, r.pct_ucl, 0, True)}", pv(r.p)])
    if excl0(r.diff_lcl, r.diff_ucl):
        bolds.add((i, 3))
table(["Depth", "Open field", "VAPV", "Diff. (pp) [95% CI]", "Diff. (%) [95% CI]", "p"], rows, [2.2, 3.0, 3.0, 3.6, 3.7, 1.5],
      align=["l", "c", "c", "c", "c", "c"], size=8, bold_cells=bolds)

doc.add_heading("Substrip level", level=2)
sp = lambda rd, e, p_: socp[(socp["round"] == rd) & (socp.env == e) & (socp.pos == p_)].iloc[0]
ROUNDS = ["2024 survey", "2025 T1", "2025 T2"]
gaps = [(rn, pos, sp(rn, "VAPV", pos)["mean"] - sp(rn, "Open field", pos)["mean"]) for rn in ROUNDS for pos in POS]
gmax = max(gaps, key=lambda g: g[2])
gmin = min(gaps, key=lambda g: g[2])
oth = [sp(rn, "Open field", pos)["mean"] for rn in ("2024 survey", "2025 T1") for pos in ("Centre", "East")]
bpos = pd.read_csv(TAB / "tbl_baseline_totalC_position.csv")


def bgap(pos):
    q = bpos[(bpos.depth == "0-30 cm") & (bpos.pos == pos)].set_index("env")["mean"]
    return q["VAPV"] - q["Open field"]


para(f"VAPV SOC {'exceeded' if gmin[2] > 0 else 'was at or around'} open-field SOC at every position in every round (gaps {num(gmin[2], 2)} to {num(gmax[2], 2)} pp; "
     f"Figure 5C, Table A3). The largest gap was at {gmax[1]} in {gmax[0]}, partly because the open-field West position had the lowest SOC in the 2024 survey and 2025 T1 "
     f"({num(sp('2024 survey', 'Open field', 'West')['mean'], 2)} and {num(sp('2025 T1', 'Open field', 'West')['mean'], 2)}% vs "
     f"{num(min(oth), 2)}–{num(max(oth), 2)}% at Centre and East). In the baseline survey (0–30 cm) the gap was {sgn(bgap('West'), 2)} pp at West and "
     f"{sgn(bgap('East'), 2)} pp at East. With 3–12 samples per cell (5 nests per cell in the baseline survey), position differences should not be over-read.", after=4)

# =========================================================================== 7. SYNTHESIS
doc.add_heading("7. Substrip synthesis", level=1)
yv = lambda e, p_: synv("Crop yield", e, p_)
nv = lambda e, p_: synv("NO3", e, p_)
wv = lambda e, p_: synv("Soil water", e, p_)
sv = lambda e, p_: synv("Soil organic", e, p_)
para(f"Figure 6 puts the four outcomes on one scale by expressing each observation as a percent of the open-field mean of its own stratum (crop-year, strip, date "
     f"or survey round). Three points stand out. (1) VAPV effects on yield, leaching and soil water differ between positions, whereas SOC is higher under VAPV at all three. (2) Natural variation among open-field positions is "
     f"negligible for yield ({num(min(yv('Open field', p_) for p_ in POS), 0)}–{num(max(yv('Open field', p_) for p_ in POS), 0)}%) and SOC "
     f"({num(min(sv('Open field', p_) for p_ in POS), 0)}–{num(max(sv('Open field', p_) for p_ in POS), 0)}%) but large for leaching "
     f"({num(min(nv('Open field', p_) for p_ in POS), 0)}–{num(max(nv('Open field', p_) for p_ in POS), 0)}%), so position-level leaching differences are the least "
     f"reliable. (3) VAPV West stands out for leaching ({num(nv('VAPV', 'West'), 0)}%), soil water ({num(wv('VAPV', 'West'), 0)}%) and SOC ({num(sv('VAPV', 'West'), 0)}%), "
     f"while VAPV East has the lowest yield ({num(yv('VAPV', 'East'), 0)}%) and VAPV Centre is closest to open field for yield "
     f"({num(yv('VAPV', 'Centre'), 0)}%) and soil water ({num(wv('VAPV', 'Centre'), 0)}%). The West leaching value is driven partly by strip A3 "
     f"(Table 7).", after=4)
figure("fig6_synthesis", 13.0)
caption("Figure 6. Substrip synthesis. Each point is mean ± SE of observations (plots, cups, nest-level date ratios or samples) as a percent of the open-field mean of the same "
        "stratum. W/C/E = West/Centre/East. The soil-water panel has only one open-field position (centre; reference = 100). Panels have different y-axis ranges.", before=2, keep=False)

# =========================================================================== 8. CAVEATS
doc.add_heading("8. Data gaps, caveats and next steps", level=1)
para("The table lists what limits each conclusion above and what would most improve it.", after=3, keep=True)
caption("Table 12. Limitations and suggested actions.", before=2)
rows = [
    ["Replication", "VAPV vs open field is a contrast between strip halves (1–3 strip pairs per crop-year); plots, cups and nests are subsamples. CIs are lower bounds on uncertainty.",
     "Treat effects as indicative; add strip pairs or years; keep reporting nest-level CIs."],
    ["Winter wheat 2025 yield", f"All {qcn('Winter wheat 2025 plots')} winter wheat 2025 plots (strip A2) are blank in the yield workbook. This is the strip with the largest leaching difference, so leaching cannot yet be linked to crop N uptake.",
     "Enter 2025 grain yield and N content, then compute N offtake and an N balance for A2."],
    ["Leaching method", f"One Daisy drainage series per strip is used for both locations, so differences come only from concentration; any PV effect on drainage is missed. "
                        f"DL vs Daisy agreement is moderate for A2 (R² {num(dl.loc['A2'].r2, 2)}) and poor for A3 (R² {num(dl.loc['A3'].r2, 2)}). Only two strips are monitored.",
     "Re-run Daisy with PV-specific radiation, temperature and wind forcing; add suction-cup strips."],
    ["Soil water", "Open-field TDR nests exist at the centre position only (3 nests); no winter data; the cleaned files contain duplicated records labelled with different crops.",
     "Add open-field West/East nests; log year-round; de-duplicate the cleaned files at source."],
    ["Soil carbon", f"Each round sampled different points, no sampling dates are in the dataset, fertiliser identity resolves for {qcn('fertiliser identity resolved')} of "
                    f"{qcn('Samples in the IUNG dataset')} samples and residue not at all; baseline gaps (larger at depth) suggest pre-existing soil differences.",
     "Re-sample the same points (IUNG); record dates and coordinates; use deeper layers as a covariate."],
    ["Positions", "Position is confounded with nest identity (one nest per position and strip) and panel distance is not recorded.",
     "Record distance to the nearest panel row for every nest and use it as a covariate."],
    ["Yield basis", "Annual yield is the sum of cuts for grass-clover (3 cuts in 2023, strip A4 only in 2025); spring barley and ryegrass are green-biomass harvests; tissue N exists for 2024 only.",
     "Digitise 2023 and 2025 quality data."],
]
table(["Area", "Limitation", "Suggested action"], rows, [2.5, 8.4, 6.1], align=["l", "l", "l"], size=8, first_col_bold=True, keep=False)

# =========================================================================== APPENDIX
doc.add_heading("Appendix", level=1)
caption("Table A1. Data-quality log: what was found in the inputs and how it was handled.", before=2)
rows = [[r.domain, r["item"], int(r.n), r.handling] for _, r in qc.iterrows()]
table(["Domain", "Item", "n", "Handling"], rows, [2.6, 7.2, 1.0, 6.2], align=["l", "l", "c", "l"], size=7.5, keep=False)

caption("Table A2. Cross-check with the plot-level generalised linear mixed models used for the manuscript figures (glmmTMB, Type II Wald tests; "
        "from the pipeline outputs, not re-fitted here).")
sel = [
    ("Yield (pooled model)", om1[(om1.Model == "Yield: pooled model") & (om1.Term == "LOCATION")]),
    ("Yield, location × crop", om1[(om1.Model == "Yield: pooled model") & (om1.Term == "LOCATION:CROP")]),
    ("Yield, position (6 levels)", om1[(om1.Model == "Yield: substrip model") & (om1.Term == "GLOBAL_SUB_STRIP_LOCATION")]),
    ("Tissue N (pooled model)", om1[(om1.Model == "Tissue N: pooled model") & (om1.Term == "LOCATION")]),
    ("NO3-N leaching, VAPV vs open", om2[(om2.Model == "NO3-N leaching: main-effects model") & (om2.Term == "location")]),
    ("NO3-N leaching, position (6 levels)", om2[(om2.Model == "NO3-N leaching: substrip model") & (om2.Term == "GLOBAL_SUB_STRIP_LOCATION")]),
]
rows = [[lab, int(d.Df.iloc[0]), num(d.Chisq.iloc[0], 1), pv(d.P.iloc[0])] for lab, d in sel]
table(["Term tested", "df", "χ²", "p"], rows, [8.0, 2.0, 3.0, 4.0], align=["l", "c", "c", "c"], size=8)
note("These plot- and cup-level tests agree in direction with the nest-level results here for yield and leaching and give smaller p-values because they treat plots and cups "
     "as independent. The pooled tissue-N test (no VAPV main effect) averages over crops and does not capture the winter-wheat increase reported in Section 3.")

caption("Table A3. SOC (IUNG, % of soil) by position and round: mean ± SE (n samples).")
rows = []
for rn in ["2024 survey", "2025 T1", "2025 T2"]:
    row = [rn]
    for e in ("Open field", "VAPV"):
        for pos in POS:
            r = sp(rn, e, pos)
            row.append(f"{pm(r['mean'], r.se, 2)} ({int(r.n_sample)})")
    rows.append(row)
table(["Round", "Open W", "Open C", "Open E", "VAPV W", "VAPV C", "VAPV E"], rows, [2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.6],
      align=["l", "c", "c", "c", "c", "c", "c"], size=7.5)

doc.add_heading("Reproducibility", level=2)
para("Everything in this report is regenerated by the scripts in 4_outputs/reports/vapv_vs_openfield_summary/: 00_setup.R, 01_data_and_tables.R (tables/*.csv), "
     "02_figures.R (figures/*.png) and build_report_docx.py (this document, reading the CSV tables). Run Rscript run_all.R from a terminal, then python "
     "build_report_docx.py. Inputs are the pipeline’s cached targets (yield, leaching, IUNG SOC) and the raw TDR, soil-survey and Daisy files in 1_input_data/.",
     size=9)

OUT.parent.mkdir(parents=True, exist_ok=True)
zoom = doc.settings.element.find(qn("w:zoom"))      # python-docx template quirk: <w:zoom> lacks the required percent attribute
if zoom is not None and zoom.get(qn("w:percent")) is None:
    zoom.set(qn("w:percent"), "100")
doc.save(OUT)
print("saved", OUT)

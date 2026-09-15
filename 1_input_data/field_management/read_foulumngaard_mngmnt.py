#!/usr/bin/env python3
"""
read_foulumngaard_mngmnt.py
============================
Reads Foulumgaard (V4F) field management records from Value4Farm Excel files
and produces two output tables:

  1. management_actions.csv  – key management events (soil prep, sowing,
                               fertilizing, spraying, harvesting) per strip/date
  2. crop_calendar.csv       – inferred crop occupancy per strip with
                               start/end dates and season year

The script is designed to be re-run whenever a new Handlinger_YYYY.xlsx file
is added to the COMPLETE_YEAR_MNGMT sub-folder.

Usage:
    python read_foulumngaard_mngmnt.py [--dir PATH] [--out PATH]

Defaults:
    --dir  <script_dir>/COMPLETE_YEAR_MNGMT
    --out  <script_dir>   (same folder as this script)
"""

import argparse
import os
import re
import shutil
import sys
import tempfile
from pathlib import Path

import pandas as pd
from typing import Dict, List, Optional, Tuple


# ---------------------------------------------------------------------------
# Crop code → English name mapping
# (covers abbreviated Danish codes and full names seen in the files)
# ---------------------------------------------------------------------------
CROP_MAP: Dict[str, Optional[str]] = {
    # Abbreviated Danish codes
    "kg":           "Grass-Clover",       # Kløvergræs
    "vh":           "Winter Wheat",        # Vinterhvede
    "by":           "Spring Barley",       # Byg (forår)
    "hv":           "Spring Wheat",        # Hvede
    "ra":           "Oilseed Rape",        # Raps
    "rug":          "Winter Rye",
    "ru":           "Winter Rye",
    "ært":          "Field Peas",
    "æt":           "Field Peas",
    "soy":          "Soybean",
    "lu":           "Lupin",
    # Ryegrass cover crop (undersown in spring barley — multiple spellings)
    "rg":                    "Ryegrass (undersown)",
    "rajgræs":               "Ryegrass (undersown)",
    "rajgrass":              "Ryegrass (undersown)",
    "raygrass":              "Ryegrass (undersown)",
    "ryegrass":              "Ryegrass (undersown)",
    "ryegrass (undersown)":  "Ryegrass (undersown)",
    # Full English/mixed names found in the files
    "soybean":      "Soybean",
    "winterwheat":  "Winter Wheat",
    "winter wheat": "Winter Wheat",
    "clovergrass":  "Grass-Clover",
    "grass-clover": "Grass-Clover",
    "springbarley": "Spring Barley",
    "spring barley":"Spring Barley",
    "barley":       "Spring Barley",
    # Entries that are NOT crops (flag as None to skip)
    "manure":       None,
    "nan":          None,
    "":             None,
}

# Ordered list of (keyword, crop_name) pairs searched in sowing operation text.
# When a sowing event's Crop column in Excel doesn't match what's actually being
# sown (e.g. "Sowing Clovergrass" with Crop="Winter Wheat" — a common data entry
# error when the previous crop's code is left in), an ADDITIONAL sowing record is
# created for the crop mentioned in the operation text.
# Also handles undersown ryegrass cover crop detected the same way.
# Order matters: first match wins; more specific terms before broader ones.
OP_SOWING_CROP_HINTS: List[Tuple[str, str]] = [
    # Ryegrass cover crop (undersown in spring barley)
    ("rajgræs",      "Ryegrass (undersown)"),
    ("rajgrass",     "Ryegrass (undersown)"),
    ("raygrass",     "Ryegrass (undersown)"),
    ("ryegrass",     "Ryegrass (undersown)"),
    ("ray grass",    "Ryegrass (undersown)"),
    # Grass-clover (sown after winter wheat — Crop column often left as WW)
    ("clovergrass",  "Grass-Clover"),
    ("kløvergræs",   "Grass-Clover"),
    ("kloevergras",  "Grass-Clover"),
    ("grass-clover", "Grass-Clover"),
    # Spring barley (safeguard against wrong Crop codes)
    ("barley",       "Spring Barley"),
    # Soybean
    ("soybean",      "Soybean"),
    ("soya",         "Soybean"),
]

# Crops that play catch-crop / undersown roles — excluded from sequence checks.
CATCH_CROPS: set = {"Ryegrass (undersown)"}

# Expected MAIN crop sequence per strip (from first year with records onward).
# Derived from the V4F General Crop Rotation table (HyPErFarm/Value4Farm,
# NEW 1-yr Grass-Clover version).  2023 is not included because the 2023
# management Excel lacks a Value4Farm sheet.
# B strips inferred from the same rotation: B1~A3, B2~A1, B3~A3, B4~A2, B5~A1.
EXPECTED_MAIN_CROP_SEQUENCE: Dict[str, List[str]] = {
    "A1": ["Winter Wheat",  "Grass-Clover",  "Spring Barley", "Soybean",       "Winter Wheat",  "Grass-Clover"],
    "A2": ["Soybean",       "Winter Wheat",  "Grass-Clover",  "Spring Barley", "Soybean",       "Winter Wheat"],
    "A3": ["Grass-Clover",  "Spring Barley", "Soybean",       "Winter Wheat",  "Grass-Clover",  "Spring Barley"],
    "A4": ["Winter Wheat",  "Grass-Clover",  "Spring Barley", "Soybean",       "Winter Wheat",  "Grass-Clover"],
    "A5": ["Grass-Clover",  "Spring Barley", "Soybean",       "Winter Wheat",  "Grass-Clover",  "Spring Barley"],
    "B1": ["Grass-Clover",  "Spring Barley", "Soybean",       "Winter Wheat",  "Grass-Clover",  "Spring Barley"],
    "B2": ["Winter Wheat",  "Grass-Clover",  "Spring Barley", "Soybean",       "Winter Wheat",  "Grass-Clover"],
    "B3": ["Grass-Clover",  "Spring Barley", "Soybean",       "Winter Wheat",  "Grass-Clover",  "Spring Barley"],
    "B4": ["Soybean",       "Winter Wheat",  "Grass-Clover",  "Spring Barley", "Soybean",       "Winter Wheat"],
    "B5": ["Winter Wheat",  "Grass-Clover",  "Spring Barley", "Soybean",       "Winter Wheat",  "Grass-Clover"],
}

# Keywords for classifying management operations (order matters: first match wins)
OPERATION_CATEGORIES: List[Tuple[str, List[str]]] = [
    ("Sowing",           ["sow", "sowing", "såed", "sått", "planted", "seeding",
                          "roller past after sowing"]),
    ("Harvest",          ["harvest", "høst", "høstet", "green 3m", "samples are made",
                          "removed and put in sacks"]),
    ("Soil preparation", ["cultivat", "harrow", "rotary", "plow", "plough", "till",
                          "tillage", "stubble", "seedbed", "cultived", "flail cutter",
                          "chopped and crushed"]),
    ("Spraying",         ["spray", "herbicide", "fungicide", "insect", "round up",
                          "fighter", "nuance", "starane", "basagran", "balaya",
                          "atlantis", "topik", "kvikup"]),
    ("Fertilizing",      ["fertiliz", "slurry", "manure", "npk", "digestate",
                          "patentkali", "yara", "extran", "gylle", "fertilizer"]),
]

# Pattern to identify Value4Farm sheets
V4F_PATTERN = re.compile(r"value4farm", re.IGNORECASE)

# Strip IDs look like "A2", "B4", "A12" etc.
STRIP_PATTERN = re.compile(r"^[A-Za-z]\d+$")


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

def classify_operation(text: str) -> str:
    """Return the operation category for a given free-text observation."""
    if not isinstance(text, str) or not text.strip():
        return "Other"
    t = text.lower()
    for category, keywords in OPERATION_CATEGORIES:
        if any(k in t for k in keywords):
            return category
    return "Other"


def normalize_crop(raw) -> Optional[str]:
    """Translate raw crop code/name to a standardised English name.

    Returns None if the value is not a valid crop (e.g. 'Manure')
    or if it contains '+' (multi-crop sampling notation like
    'WinterWheat+Clovergrass+Springbarley' which spans multiple strips).
    """
    if raw is None or (isinstance(raw, float) and pd.isna(raw)):
        return None
    s = str(raw).strip()
    if not s or s.lower() == "nan":
        return None

    # Multi-crop strings (e.g. sampling events covering all strips at once)
    if "+" in s:
        return None

    # Direct lookup (case-insensitive)
    key = s.lower()
    if key in CROP_MAP:
        return CROP_MAP[key]

    # Prefix match (handles things like "Winter Wheat'25")
    for pattern, name in CROP_MAP.items():
        if key.startswith(pattern):
            return name

    # Return original title-cased if not in the map
    return s.title()


def parse_rotation(rotation) -> list[str]:
    """Expand 'A2+B4' → ['A2', 'B4'], filtering out non-strip tokens."""
    if rotation is None or (isinstance(rotation, float) and pd.isna(rotation)):
        return []
    parts = [p.strip() for p in str(rotation).split("+")]
    strips = [p for p in parts if STRIP_PATTERN.match(p)]
    # Fall back to all parts if none match strip pattern (shouldn't happen)
    return strips if strips else []


def find_header_row(df_raw: pd.DataFrame) -> Optional[int]:
    """Locate the row index whose first non-null value is 'Date' or 'Dato'."""
    for i, row in df_raw.iterrows():
        vals = [str(v).strip() for v in row.values if not pd.isna(v)]
        if vals and vals[0].lower() in ("date", "dato"):
            return i
    return None


def read_v4f_sheet(filepath: Path) -> pd.DataFrame:
    """Read the Value4Farm sheet from one Excel file and return a clean DataFrame."""
    tmp = tempfile.mktemp(suffix=".xlsx")
    try:
        shutil.copy2(str(filepath), tmp)
        xl = pd.ExcelFile(tmp)

        # Find the Value4Farm sheet
        sheet = next((s for s in xl.sheet_names if V4F_PATTERN.search(s)), None)
        if sheet is None:
            print(f"  [skip] No Value4Farm sheet in {filepath.name}")
            return pd.DataFrame()

        df_raw = xl.parse(sheet, header=None)
        hrow = find_header_row(df_raw)
        if hrow is None:
            print(f"  [skip] Header row not found in {filepath.name} / {sheet}")
            return pd.DataFrame()

        df = xl.parse(sheet, header=hrow)
        df.columns = [str(c).strip() for c in df.columns]

        # Map column names flexibly
        col_map: Dict[str, str] = {}
        for c in df.columns:
            cl = c.lower()
            if c not in col_map.values():
                if cl in ("date", "dato") and "date" not in col_map.values():
                    col_map[c] = "date"
                elif cl == "rotation" and "rotation" not in col_map.values():
                    col_map[c] = "rotation"
                elif cl == "crop" and "crop" not in col_map.values():
                    col_map[c] = "crop"
                elif ("operation" in cl or "observation" in cl) and "operation" not in col_map.values():
                    col_map[c] = "operation"

        keep = [c for c in col_map if c in df.columns]
        df = df[keep].rename(columns=col_map)

        # Add missing columns as NaN
        for col in ("date", "rotation", "crop", "operation"):
            if col not in df.columns:
                df[col] = pd.NA

        # Remove "Week X" rows and rows where date is not a real date
        df = df[~df["date"].astype(str).str.strip().str.lower().str.startswith("week")]
        df["date"] = pd.to_datetime(df["date"], errors="coerce")
        df = df.dropna(subset=["date"]).copy()

        # Attach source metadata
        year_match = re.search(r"\d{4}", filepath.stem)
        df["source_year"] = int(year_match.group()) if year_match else None
        df["source_file"] = filepath.name
        df["sheet"] = sheet

        return df[["date", "rotation", "crop", "operation", "source_year", "source_file", "sheet"]]

    except Exception as e:
        print(f"  [error] {filepath.name}: {e}")
        return pd.DataFrame()
    finally:
        try:
            os.unlink(tmp)
        except OSError:
            pass


# ---------------------------------------------------------------------------
# Core processing
# ---------------------------------------------------------------------------

def build_actions(df: pd.DataFrame) -> pd.DataFrame:
    """
    Expand each record to individual strips, translate crop codes,
    and classify the management operation.
    """
    rows = []
    for _, r in df.iterrows():
        strips = parse_rotation(r.get("rotation"))
        if not strips:
            continue

        crop_raw = r.get("crop")
        crop_name = normalize_crop(crop_raw)
        op_text = r.get("operation")
        op_type = classify_operation(str(op_text) if op_text is not None else "")
        op_str = (str(op_text).strip()
                  if op_text is not None and not (isinstance(op_text, float) and pd.isna(op_text))
                  else None)

        for strip in strips:
            rows.append({
                "source_year":     r.get("source_year"),
                "date":            r["date"].date(),
                "strip":           strip,
                "crop_code":       str(crop_raw).strip() if not pd.isna(crop_raw) else None,
                "crop_name":       crop_name,
                "operation_type":  op_type,
                "operation_detail": op_str,
                "source_file":     r.get("source_file"),
            })

            # If the sowing operation text mentions a recognisable crop that differs
            # from the Crop column, create an ADDITIONAL sowing record for that
            # crop.  This corrects data-entry errors (e.g. "Sowing Clovergrass"
            # with Crop = "Winter Wheat") and detects undersown cover crops (e.g.
            # "Sowing Rajgræs" alongside Spring Barley).
            if op_type == "Sowing" and op_str:
                op_lower = op_str.lower()
                for hint_kw, hint_crop in OP_SOWING_CROP_HINTS:
                    if hint_kw in op_lower:
                        if hint_crop != crop_name:
                            rows.append({
                                "source_year":      r.get("source_year"),
                                "date":             r["date"].date(),
                                "strip":            strip,
                                "crop_code":        hint_kw,
                                "crop_name":        hint_crop,
                                "operation_type":   "Sowing",
                                "operation_detail": op_str,
                                "source_file":      r.get("source_file"),
                            })
                        break  # Only one extra record per sowing event

    return pd.DataFrame(rows)


def build_crop_calendar(actions: pd.DataFrame) -> pd.DataFrame:
    """
    Infer a crop calendar: for each (strip, crop_name) combination,
    determine the best estimate of when the crop occupied the strip.

    Logic:
    - start_date = date of explicit sowing event (preferred) or first mention
    - end_date   = date of explicit harvest event (preferred) or last mention
    - After building the initial table, end_dates of 'last_mention' periods are
      extended to the eve of the next sowing in the same strip (if one exists).
    - season_year = harvest year (or year of last recorded activity)
    """
    if actions.empty:
        return pd.DataFrame()

    # Work only with rows that have a recognised crop
    valid = actions[actions["crop_name"].notna()].copy()
    valid["date"] = pd.to_datetime(valid["date"])

    cal_rows = []
    for (strip, crop), grp in valid.groupby(["strip", "crop_name"]):
        grp = grp.sort_values("date")

        sow_rows  = grp[grp["operation_type"] == "Sowing"]
        harv_rows = grp[grp["operation_type"] == "Harvest"]

        start_date = sow_rows["date"].min()  if not sow_rows.empty  else grp["date"].min()
        start_type = "sowing"                if not sow_rows.empty  else "first_mention"
        end_date   = harv_rows["date"].max() if not harv_rows.empty else grp["date"].max()
        end_type   = "harvest"               if not harv_rows.empty else "last_mention"
        season_year = int(end_date.year)

        cal_rows.append({
            "strip":        strip,
            "crop_name":    crop,
            "season_year":  season_year,
            "start_date":   start_date.date(),
            "start_type":   start_type,
            "end_date":     end_date.date(),
            "end_type":     end_type,
            "n_records":    len(grp),
        })

    cal = (pd.DataFrame(cal_rows)
             .sort_values(["strip", "start_date"])
             .reset_index(drop=True))

    # --- Extend 'last_mention' end dates to eve of next sowing in same strip ---
    cal["start_date"] = pd.to_datetime(cal["start_date"])
    cal["end_date"]   = pd.to_datetime(cal["end_date"])

    # Collect all sowing dates per strip (from actions, not just cal)
    sow_dates = (
        valid[valid["operation_type"] == "Sowing"]
        .groupby("strip")["date"]
        .apply(lambda x: sorted(x.unique()))
        .to_dict()
    )

    for idx, row in cal.iterrows():
        if row["end_type"] != "last_mention":
            continue
        strip_sowings = sow_dates.get(row["strip"], [])
        # Find the first sowing date that is AFTER this period's start
        next_sowings = [d for d in strip_sowings if d > row["end_date"]]
        if next_sowings:
            cal.at[idx, "end_date"] = min(next_sowings) - pd.Timedelta(days=1)

    cal["start_date"] = cal["start_date"].dt.date
    cal["end_date"]   = cal["end_date"].dt.date

    return cal


# ---------------------------------------------------------------------------
# Coherence check
# ---------------------------------------------------------------------------

def check_calendar_coherence(calendar: pd.DataFrame) -> None:
    """Compare generated crop calendar against expected V4F rotation sequence.

    For each A/B strip, the observed main-crop sequence (excluding catch crops)
    is checked against EXPECTED_MAIN_CROP_SEQUENCE.  A prefix match is accepted
    because the calendar covers only a partial rotation cycle.
    """
    print("\n" + "=" * 70)
    print("COHERENCE CHECK  —  crop sequence vs. expected V4F rotation")
    print("=" * 70)

    strips_to_check = sorted(EXPECTED_MAIN_CROP_SEQUENCE.keys())
    all_pass = True

    for strip in strips_to_check:
        if strip not in calendar["strip"].values:
            print(f"  {strip}: MISSING — no calendar entries found")
            all_pass = False
            continue

        expected = EXPECTED_MAIN_CROP_SEQUENCE[strip]

        # Main-crop sequence in chronological order (by sowing / first mention)
        strip_cal = calendar[
            (calendar["strip"] == strip) &
            (~calendar["crop_name"].isin(CATCH_CROPS))
        ].sort_values("start_date")

        observed: List[str] = strip_cal["crop_name"].tolist()

        if not observed:
            print(f"  {strip}: MISSING — only catch-crop entries found")
            all_pass = False
            continue

        # Check that observed is a valid prefix of the expected sequence
        mismatches: List[str] = []
        for i, (obs, exp) in enumerate(zip(observed, expected)):
            if obs != exp:
                mismatches.append(
                    f"  position {i + 1}: found '{obs}', expected '{exp}'"
                )

        if len(observed) > len(expected):
            mismatches.append(
                f"  {len(observed)} crops observed but only {len(expected)} expected"
            )

        if mismatches:
            all_pass = False
            print(f"  {strip}: WARN")
            print(f"    observed : {' -> '.join(observed)}")
            print(f"    expected : {' -> '.join(expected[:len(observed)])}")
            for m in mismatches:
                print(f"    {m}")
        else:
            print(f"  {strip}: OK   ({' -> '.join(observed)})")

    print("")
    if all_pass:
        print("All strips match the expected rotation sequence.")
    else:
        print("WARNING: One or more strips deviate from the expected rotation.")


# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--dir", default=None,
                        help="Directory containing Handlinger_*.xlsx files "
                             "(default: COMPLETE_YEAR_MNGMT next to this script)")
    parser.add_argument("--out", default=None,
                        help="Output directory for CSV files "
                             "(default: same directory as this script)")
    args = parser.parse_args()

    script_dir = Path(__file__).parent
    mngmt_dir  = Path(args.dir) if args.dir else script_dir / "COMPLETE_YEAR_MNGMT"
    out_dir    = Path(args.out) if args.out else script_dir

    print(f"Reading management files from : {mngmt_dir}")
    print(f"Writing output CSV files to   : {out_dir}\n")

    # ----- Read all Handlinger_*.xlsx files ----------------------------------
    files = sorted(mngmt_dir.glob("Handlinger_*.xlsx"))
    if not files:
        print("ERROR: No Handlinger_*.xlsx files found. Exiting.")
        sys.exit(1)

    dfs: list[pd.DataFrame] = []
    for f in files:
        print(f"Reading {f.name} ...", end=" ", flush=True)
        df = read_v4f_sheet(f)
        if not df.empty:
            dfs.append(df)
            print(f"{len(df)} rows")
        else:
            print("(no data)")

    if not dfs:
        print("ERROR: No data loaded. Exiting.")
        sys.exit(1)

    combined = pd.concat(dfs, ignore_index=True)
    print(f"\nTotal rows loaded : {len(combined)}")

    # ----- Build management actions table ------------------------------------
    print("Building management actions table ...")
    actions = build_actions(combined)

    KEY_TYPES = {"Sowing", "Harvest", "Soil preparation", "Spraying", "Fertilizing"}
    actions_key = actions[actions["operation_type"].isin(KEY_TYPES)].copy()
    actions_key = actions_key.sort_values(["strip", "date"]).reset_index(drop=True)

    actions_out = out_dir / "management_actions.csv"
    actions_key.to_csv(actions_out, index=False)
    print(f"Saved: {actions_out}  ({len(actions_key)} rows)")

    # ----- Build crop calendar -----------------------------------------------
    print("Building crop calendar ...")
    calendar = build_crop_calendar(actions)

    cal_out = out_dir / "crop_calendar.csv"
    calendar.to_csv(cal_out, index=False)
    print(f"Saved: {cal_out}  ({len(calendar)} rows)")

    # ----- Print summaries ---------------------------------------------------
    print("\n" + "=" * 70)
    print("CROP CALENDAR")
    print("=" * 70)
    print(calendar.to_string(index=False))

    print("\n" + "=" * 70)
    print("MANAGEMENT ACTIONS — count by strip / year / type")
    print("=" * 70)
    summary = (actions_key
               .groupby(["strip", "source_year", "operation_type"])
               .size()
               .reset_index(name="count")
               .sort_values(["strip", "source_year", "operation_type"]))
    print(summary.to_string(index=False))

    # ----- Coherence check ---------------------------------------------------
    check_calendar_coherence(calendar)

    print("\nDone.")


if __name__ == "__main__":
    main()

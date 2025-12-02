#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime
import matplotlib.dates as mdates


# -------------------------------------------------------
# Utility: merge and sort anthropological reference lines
# -------------------------------------------------------
def merge_and_sort_reference_lines(refdict):
    grouped = {}
    for label, pct in refdict.items():
        grouped.setdefault(pct, []).append(label)

    sorted_pcts = sorted(grouped.keys())

    merged = {}
    for pct in sorted_pcts:
        labels = grouped[pct]
        combined_label = ", ".join(labels)
        merged[f"{combined_label} (~{pct}%)"] = pct
    return merged


# -------------------------------------------------------
# Simulation function
# -------------------------------------------------------
def simulate_adipose_LA_from_pairs(
    readings,                   # list of (date_string, weight_kg)
    initial_bodyfat_frac=0.30,
    initial_LA_frac=0.25,
    LA_mobilization_factor=0.85,
    LA_regain_frac=0.0
):

    # ---- Sort readings by date, just in case ----
    readings = sorted(readings, key=lambda x: x[0])

    # ---- Extract dates and weights ----
    date_strings = [r[0] for r in readings]
    weights = np.array([r[1] for r in readings])

    # ---- Convert date strings to datetime objects ----
    dates = [datetime.strptime(d, "%Y-%m-%d") for d in date_strings]

    # ---- Initial body composition ----
    initial_weight = weights[0]
    initial_fat_mass = initial_weight * 1000 * initial_bodyfat_frac
    lean_mass = initial_weight * 1000 - initial_fat_mass   # FIXED LEAN MASS

    # ---- Fat mass inferred from weights ----
    fat_masses = weights * 1000 - lean_mass
    fat_masses = np.maximum(fat_masses, 0)

    # ---- Fat gain/loss events ----
    events = np.diff(fat_masses)

    # ---- Initialize simulation state ----
    fat_mass = [fat_masses[0]]
    LA_mass = [fat_masses[0] * initial_LA_frac]
    LA_percent = [initial_LA_frac]
    nonLA_mass = [fat_masses[0] - LA_mass[0]]
    date_axis = [dates[0]]

    # ---- Simulation loop ----
    for i, change in enumerate(events):

        F = fat_mass[-1]
        LA = LA_mass[-1]
        nonLA = F - LA

        if change < 0:
            # ------ FAT LOSS ------
            loss = -change
            loss = min(loss, F)

            denom = nonLA + LA * LA_mobilization_factor
            if denom == 0:
                break

            k = loss / denom
            x = LA * k * LA_mobilization_factor   # LA burned
            y = nonLA * k                         # non-LA burned

            LA_new = LA - x
            nonLA_new = nonLA - y

        else:
            # ------ FAT GAIN ------
            gain = change
            gained_LA = gain * LA_regain_frac
            gained_nonLA = gain - gained_LA

            LA_new = LA + gained_LA
            nonLA_new = nonLA + gained_nonLA

        F_new = LA_new + nonLA_new

        fat_mass.append(F_new)
        LA_mass.append(LA_new)
        nonLA_mass.append(nonLA_new)
        LA_percent.append(LA_new / F_new if F_new > 0 else 0)
        date_axis.append(dates[i+1])

    return (
        np.array(date_axis),
        np.array(fat_mass),
        np.array(nonLA_mass),
        np.array(LA_mass),
        np.array(LA_percent),
        lean_mass
    )



# -------------------------------------------------------
# Anthropological reference LA% lines (RAW)
# -------------------------------------------------------
reference_lines_raw = {
    "Inuit": 1.5,
    "Masai": 2.5,
    "Victorian English": 2.5,
    "San Bushmen": 3,
    "Hadza": 3,
    "Kitavans": 3,
    "Pre-industrial Europeans": 3.5,
    "Hunter–gatherer": 4,
    "Okinawans pre-1950": 4,
    "1950s Americans": 7,
    "Modern Mediterranean": 6,
    "1970s Americans": 9,
    "Modern China": 10,
    "Modern British": 12,
    "Modern American": 15
}

# Merge + sort lines
reference_lines = merge_and_sort_reference_lines(reference_lines_raw)



# -------------------------------------------------------
# INPUT DATA
# -------------------------------------------------------
readings = [
    ("2023-05-22", 99),
    ("2023-09-14", 92+1.5),
    ("2023-10-13", 95.46),
    ("2023-11-11", 91.6+1.5),
    ("2024-04-05", 97.44),
    ("2024-05-05", 95.50),
    ("2024-08-28", 97.16),
    ("2024-09-22", 94.88+1.5),
    ("2024-10-06", 96.6),
    ("2024-11-03", 93.4+1.5),
    ("2024-12-29", 96.6),
    ("2025-01-14", 94.07),
    ("2025-03-28", 96.94),
    ("2025-07-01", 92.37),
    ("2025-09-15", 96.2),
    ("2025-11-23", 91.2+1.5),
]



# -------------------------------------------------------
# RUN SIMULATION
# -------------------------------------------------------
dates, fat_mass, nonLA_mass, LA_mass, LA_percent, lean_mass = simulate_adipose_LA_from_pairs(
    readings,
    initial_bodyfat_frac=0.30,
    initial_LA_frac=0.25
)

# -------------------------------------------------------
# FIGURE: Stacked composition + LA% line + reference lines
# -------------------------------------------------------

lean_mass_kg = lean_mass / 1000
LA_mass_kg = LA_mass / 1000
nonLA_mass_kg = nonLA_mass / 1000

lean_layer = lean_mass_kg * np.ones_like(LA_mass_kg)
nonLA_layer = nonLA_mass_kg
LA_layer = LA_mass_kg

fig, ax1 = plt.subplots(figsize=(14,7))

# ----- Stacked area plot -----
ax1.stackplot(
    dates,
    lean_layer,
    nonLA_layer,
    LA_layer,
    labels=["Lean Mass", "Non-LA Fat Mass", "LA Fat Mass"],
    colors=["#88c999", "#f2c572", "#e05f5f"],
    alpha=0.95
)

ax1.set_xlabel("Date")
ax1.set_ylabel("Mass (kg)")
ax1.set_title("Body Composition With Adipose LA% and Anthropological Reference Lines")

# Date formatting
ax1.xaxis.set_major_locator(mdates.AutoDateLocator())
ax1.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
plt.xticks(rotation=45)
ax1.grid(True, alpha=0.3)

# ----- Right axis: LA% line (auto-scaled) -----
ax2 = ax1.twinx()
ax2.plot(dates, LA_percent, color="black", linewidth=2, label="Adipose LA%")
ax2.set_ylabel("Adipose LA Fraction")

LA_min = 0
LA_max = max(LA_percent) * 1.05
ax2.set_ylim(LA_min, LA_max)

# ----- Horizontal reference lines + labels INSIDE green area -----

# Find x-position slightly inside the stackplot (5% of plot width)
x0 = dates[0]
x1 = dates[-1]
label_x = x0 + (x1 - x0) * 0.03   # 3% into the plot from the left

for label, pct in reference_lines.items():
    y = pct / 100
    ax2.axhline(y, color="black", linestyle=":", linewidth=0.7)
    ax2.text(
        label_x, y,
        label,
        va="center",
        ha="left",
        fontsize=8,
        color="black",
        bbox=dict(facecolor="#88c999", alpha=0.6, edgecolor="none", pad=1)
    )

fig.tight_layout()
plt.show()

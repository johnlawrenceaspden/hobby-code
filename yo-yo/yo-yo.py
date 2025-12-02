#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime
import matplotlib.dates as mdates


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



# ------------------ INPUT DATA ------------------

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


# ------------------ RUN SIMULATION ------------------

dates, fat_mass, nonLA_mass, LA_mass, LA_percent, lean_mass = simulate_adipose_LA_from_pairs(
    readings,
    initial_bodyfat_frac=0.30,
    initial_LA_frac=0.25
)


# -------------------------------------------------------
# FIGURE: Stacked body composition + LA% overlay
# -------------------------------------------------------

lean_mass_kg = lean_mass / 1000
LA_mass_kg = LA_mass / 1000
nonLA_mass_kg = nonLA_mass / 1000

# Layers
lean_layer = lean_mass_kg * np.ones_like(LA_mass_kg)
nonLA_layer = nonLA_mass_kg
LA_layer = LA_mass_kg

fig, ax1 = plt.subplots(figsize=(12,7))

# ---------- Stacked body composition ----------
ax1.stackplot(
    dates,
    lean_layer,
    nonLA_layer,
    LA_layer,
    labels=["Lean Mass", "Non-LA Fat Mass", "LA Fat Mass"],
    colors=["#88c999", "#f2c572", "#e05f5f"],   # green, amber, red
    alpha=0.95
)

ax1.set_xlabel("Date")
ax1.set_ylabel("Mass (kg)")
ax1.set_title("Body Composition Over Time (Lean + Non-LA Fat + LA Fat) with LA% Overlay")
ax1.grid(True, alpha=0.3)


# ---------- Right axis: LA% ----------
ax2 = ax1.twinx()
ax2.plot(dates, LA_percent * 100, color="black", linewidth=2, label="LA %")
ax2.set_ylabel("Adipose LA %", color="black")
ax2.tick_params(axis="y", labelcolor="black")

# Zero-base the LA% axis
ax2.set_ylim(bottom=0)

# ---------- Reference LA% lines for historical populations ----------
reference_lines = {
    "Inuit (~1.5%)": 1.5,
    "Masai (~2.5%), Victorian English (~2.5%)": 2.5,
    "San Bushmen (~3%), Hadza (~3%), Kitavans (~3%)": 3,
    "Pre-industrial Europeans (~3.5%)": 3.5,
    "Hunter–gatherer (~4%), Okinawans pre-1950 (~4%)": 4,
    "Modern Mediterranean (~6%)": 6,
    "1950s Americans (~7%)": 7,
    "1970s Americans (~9%)": 9,
    "Modern China (~10%)": 10,
    "Modern British (~12%)": 12,
    "Modern American (~15%)": 15
}



for label, value in reference_lines.items():
    ax2.axhline(
        y=value,
        color="black",
        linestyle="dotted",
        linewidth=1
    )
    ax2.text(
        dates[0], value, "  " + label,
        va="center",
        ha="left",
        fontsize=9,
        color="black"
    )




# ---------- Date formatting ----------
ax1.xaxis.set_major_locator(mdates.AutoDateLocator())
ax1.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
plt.xticks(rotation=45)

# ---------- Combined legends ----------
lines1, labels1 = ax1.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
plt.legend(lines1 + lines2, labels1 + labels2, loc="upper left")

plt.tight_layout()
plt.show()

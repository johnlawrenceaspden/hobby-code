#!/usr/bin/env python3
"""
Combined script: Program B's ODE LA model + Program A's plotting.

- Daily interpolation of weight (from readings)
- ODE-like Euler update for fat mass F(t) and LA mass L(t)
- Fixed initial lean mass (as in Program A)
- Stacked plot: lean (fixed) + non-LA fat + LA fat
- Right-axis: adipose LA % with anthropological reference lines
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import datetime, timedelta

# -----------------------
# Utility: merge & sort anthropological lines
# -----------------------
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

# -----------------------
# Anthropological reference LA% lines (RAW)
# -----------------------
reference_lines_raw = {
    "Masai": 2.5,
    "Victorian English": 2.5,
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
reference_lines = merge_and_sort_reference_lines(reference_lines_raw)

# -----------------------
# INPUT DATA (weight readings in kg)
# -----------------------
keto_water_weight = 1.5
readings = [
    ("2023-05-22", 99),
    ("2023-09-14", 92 + keto_water_weight),
    ("2023-10-13", 95.46),
    ("2023-11-11", 91.6 + keto_water_weight),
    ("2024-04-05", 97.44),
    ("2024-05-05", 95.50),
    ("2024-08-28", 97.16),
    ("2024-09-22", 94.88 + keto_water_weight),
    ("2024-10-06", 96.6),
    ("2024-11-03", 93.4 + keto_water_weight),
    ("2024-12-29", 96.6),
    ("2025-01-14", 94.07),
    ("2025-03-28", 96.94),
    ("2025-07-01", 92.37),
    ("2025-09-15", 96.2),
    ("2025-11-23", 91.2 + keto_water_weight),
]

# convert readings
dates_readings = [datetime.strptime(d, "%Y-%m-%d") for d, _ in readings]
weights_readings = [w for _, w in readings]

# -----------------------
# Interpolate to daily weight curve
# -----------------------
start = dates_readings[0]
end = dates_readings[-1]
total_days = (end - start).days
date_range = [start + timedelta(days=i) for i in range(total_days + 1)]

interp_x = [(d - start).days for d in date_range]
sample_x = [(d - start).days for d in dates_readings]
interp_weights = np.interp(interp_x, sample_x, weights_readings)

# -----------------------
# Model parameters (tweak if wanted)
# -----------------------
initial_bodyfat_frac = 0.30    # fraction of total weight that is fat at start
initial_LA_frac = 0.25         # fraction of fat that is linoleic acid at start
LA_mobilization_factor = 0.85  # rL in Program B (how strongly LA is mobilized relative to other FA)
rO = 1.0                       # other fatty acids mobilization coefficient
p_diet = 0.02                  # dietary LA fraction (2% of gained fat is LA)
half_life_days = 680.0         # endogenous LA turnover half-life (days)
k = np.log(2) / half_life_days # first-order endogenous LA loss rate (1/day)

# -----------------------
# Initialize state arrays (units: kg)
# -----------------------
n_days = len(interp_weights)
F = np.zeros(n_days)   # total fat mass (kg)
L = np.zeros(n_days)   # LA mass within fat (kg)

# initial fat and LA
F[0] = initial_bodyfat_frac * interp_weights[0]   # kg fat at t=0
L[0] = initial_LA_frac * F[0]

# Fix lean mass as in Program A (lean mass = initial weight - initial fat)
lean_mass_kg = interp_weights[0] - F[0]

# -----------------------
# Daily Euler-like update loop
# -----------------------
for i in range(1, n_days):
    dW = interp_weights[i] - interp_weights[i - 1]  # change in total body mass (kg)
    phi = -dW  # per Program B: phi > 0 means fat mobilized (weight decreasing)
    Fi = F[i - 1]
    Li = L[i - 1]

    # safety tiny values
    if Fi <= 0:
        F[i] = Fi
        L[i] = Li
        continue

    if phi > 0:
        # FAT LOSS
        denom = rO * Fi + (LA_mobilization_factor - rO) * Li
        if denom <= 0:
            # fallback: mobilize LA proportionally to its fraction of fat
            mobil = phi * (Li / max(Fi, 1e-12))
        else:
            mobil = phi * (LA_mobilization_factor * Li) / denom

        dL = -mobil - k * Li   # loss from mobilization + endogenous turnover
        dF = -phi              # fat loss equals phi (kg)

    else:
        # FAT GAIN
        gain = -phi
        dL = -k * Li + gain * p_diet  # endogenous turnover + dietary LA deposition
        dF = gain

    F[i] = max(Fi + dF, 0.0)
    L[i] = max(Li + dL, 0.0)

# -----------------------
# Derived series
# -----------------------
nonLA = F - L                      # non-LA fat mass (kg)
LA_percent = np.where(F > 0, L / F, 0.0)  # fraction (0-1)
LA_percent_pct = LA_percent * 100

# -----------------------
# Build summary table at original reading dates
# -----------------------
rows = []
for (d_str, w) in readings:
    idx = (datetime.strptime(d_str, "%Y-%m-%d") - start).days
    rows.append([d_str, w, F[idx], L[idx], 100 * (L[idx] / F[idx]) if F[idx] > 0 else 0.0])

df = pd.DataFrame(rows, columns=["date", "weight_kg", "fat_kg", "LA_kg", "LA_pct_of_fat"])
print(df.to_string(index=False))

# -----------------------
# Print final summary
# -----------------------
final_summary = {
    "initial_weight_kg": interp_weights[0],
    "initial_fat_kg": F[0],
    "initial_LA_kg": L[0],
    "final_weight_kg": interp_weights[-1],
    "final_fat_kg": F[-1],
    "final_LA_kg": L[-1],
    "final_LA_pct_of_fat": (100 * L[-1] / F[-1]) if F[-1] > 0 else 0.0
}
print("\nSummary:")
for kname, val in final_summary.items():
    print(f"  {kname}: {val:.4f}")

# -----------------------
# PLOTTING: stacked composition + LA% + reference lines
# -----------------------
# prepare layers: lean (fixed), nonLA, LA
lean_layer = lean_mass_kg * np.ones_like(F)
nonLA_layer = nonLA
LA_layer = L

fig, ax1 = plt.subplots(figsize=(14, 7))

ax1.stackplot(
    date_range,
    lean_layer,
    nonLA_layer,
    LA_layer,
    labels=["Lean Mass", "Non-LA Fat Mass", "LA Fat Mass"],
    colors=["#88c999", "#e05f5f", "#f2c572"],
    alpha=0.95
)

ax1.set_xlabel("Date")
ax1.set_ylabel("Mass (kg)")
ax1.set_title("Combined: Inferred Body Composition With Adipose LA% (ODE model + stacked plot)")

# date formatting
ax1.xaxis.set_major_locator(mdates.AutoDateLocator())
ax1.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
plt.xticks(rotation=45)
ax1.grid(True, alpha=0.3)

# show weight readings as black dots on the bottom axis for reference
ax1.scatter(dates_readings, weights_readings, color="black", s=20, zorder=5, label="Weight readings (kg)")

# right axis: LA% line
ax2 = ax1.twinx()
ax2.plot(date_range, LA_percent, color="black", linewidth=2, label="Adipose LA% (fraction)")
ax2.set_ylabel("Adipose LA Fraction (0-1)")

# limit right axis from 0 to slightly above max observed LA fraction
ax2.set_ylim(0, max(0.05, LA_percent.max() * 1.05))

# reference lines (as fractions on right axis)
x0 = date_range[0]
x1 = date_range[-1]
label_x = x0 + (x1 - x0) * 0.03  # 3% into the plot from the left

for label, pct in reference_lines.items():
    y = pct / 100.0
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

# Legends
lines1, labels1 = ax1.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax1.legend(lines1 + lines2, labels1 + labels2, loc="upper left")

fig.tight_layout()
plt.show()

# -----------------------
# LA % standalone plot (for clarity)
# -----------------------
plt.figure(figsize=(10, 4))
plt.plot(date_range, LA_percent_pct, label="LA % of adipose")
plt.title("Adipose LA % of Fat (percent)")
plt.ylabel("LA (% of adipose fat)")
plt.xlabel("Date")
plt.grid()
plt.xticks(rotation=25)
plt.tight_layout()
plt.show()

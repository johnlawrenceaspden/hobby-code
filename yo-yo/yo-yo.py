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
# FIGURE 1: Adipose LA%
# -------------------------------------------------------

plt.figure(figsize=(10,6))
plt.plot(dates, LA_percent, marker='o')

plt.xlabel("Date")
plt.ylabel("Adipose LA Fraction")
plt.title("Adipose LA% Over Time (Actual Calendar Dates)")
plt.grid(True)

plt.gca().xaxis.set_major_locator(mdates.AutoDateLocator())
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
plt.xticks(rotation=45)

plt.tight_layout()
plt.show()


# -------------------------------------------------------
# FIGURE 2: Stacked area plot (LEAN + non-LA fat + LA fat)
# -------------------------------------------------------

lean_mass_kg = lean_mass / 1000
LA_mass_kg = LA_mass / 1000
nonLA_mass_kg = nonLA_mass / 1000

lean_layer = lean_mass_kg * np.ones_like(LA_mass_kg)
nonLA_layer = nonLA_mass_kg
LA_layer = LA_mass_kg

plt.figure(figsize=(12,6))

plt.stackplot(
    dates,
    lean_layer,
    nonLA_layer,
    LA_layer,
    labels=["Lean Mass", "Non-LA Fat Mass", "LA Fat Mass"],
    colors=["#88c999", "#f2c572", "#e05f5f"],   # green, amber, red
    alpha=0.95
)

plt.xlabel("Date")
plt.ylabel("Mass (kg)")
plt.title("Stacked Body Composition Over Time (Lean + Non-LA + LA)")
plt.legend(loc="upper left")

plt.gca().xaxis.set_major_locator(mdates.AutoDateLocator())
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
plt.grid(True, alpha=0.3)
plt.xticks(rotation=45)

plt.tight_layout()
plt.show()

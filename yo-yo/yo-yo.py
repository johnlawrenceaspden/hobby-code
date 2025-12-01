#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt

def simulate_adipose_LA(
    initial_fat=30000,                # grams
    initial_LA_frac=0.25,             # fraction (e.g., 0.25 = 25%)
    events=None,                      # list of gains/losses (grams): e.g. [-5000, +3000, -4000, +2000]
    LA_mobilization_factor=0.85,      # LA mobilization relative to other fats (<1 = LA spared)
    LA_regain_frac=0.0                # LA content of regained fat (0 = LA-free)
):
    if events is None:
        events = []

    # Initial state
    fat_mass = [initial_fat]
    LA_mass = [initial_fat * initial_LA_frac]
    LA_percent = [initial_LA_frac]

    for change in events:
        F = fat_mass[-1]
        LA = LA_mass[-1]
        nonLA = F - LA

        # -------------------------
        # LOSS PHASE (negative change)
        # -------------------------
        if change < 0:
            loss = -change

            # Bound: cannot lose more fat than exists
            loss = min(loss, F)

            # Solve proportional mobilization:
            # LA burned = x
            # non-LA burned = y
            # x + y = loss
            # (x/LA) : (y/nonLA) = LA_mobilization_factor : 1
            denom = nonLA + LA * LA_mobilization_factor
            if denom == 0:
                break

            k = loss / denom
            x = LA * k * LA_mobilization_factor      # LA burned
            y = nonLA * k                            # non-LA burned

            LA_new = LA - x
            nonLA_new = nonLA - y

        # -------------------------
        # GAIN PHASE (positive change)
        # -------------------------
        else:
            gain = change
            gained_LA = gain * LA_regain_frac
            gained_nonLA = gain - gained_LA

            LA_new = LA + gained_LA
            nonLA_new = nonLA + gained_nonLA

        # Update totals
        F_new = LA_new + nonLA_new
        fat_mass.append(F_new)
        LA_mass.append(LA_new)
        LA_percent.append(LA_new / F_new if F_new > 0 else 0)

    return fat_mass, LA_mass, LA_percent


# -------------------------
# EXAMPLE USAGE
# -------------------------

events = [-6000, +2000, -2000, +4000, -3000, -2000, +4500, -3000, +2000, -2000, +2500, -4500, +3500, -1000, ]   # Custom lose/regain pattern

fat_mass, LA_mass, LA_percent = simulate_adipose_LA(
    initial_fat=30000,
    initial_LA_frac=0.25,
    events=events,
    LA_mobilization_factor=0.85,
    LA_regain_frac=0.0
)

# -------------------------
# PLOT RESULTS
# -------------------------
plt.figure(figsize=(8,5))
plt.plot(LA_percent, marker='o')
plt.xlabel("Event Number")
plt.ylabel("Adipose LA Fraction")
plt.title("Adipose LA% Over Arbitrary Loss/Regain Events")
plt.grid(True)
plt.show()

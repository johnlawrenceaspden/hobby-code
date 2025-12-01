#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt

# --- PARAMETERS ---
initial_fat = 30000          # grams
initial_LA_frac = 0.25       # starting adipose LA fraction
initial_LA = initial_fat * initial_LA_frac

cycle_loss = 10000           # grams lost per cycle
cycle_regain = 10000         # grams regained per cycle
LA_regain_frac = 0.0         # LA content of regained fat (0 = LA-free)

LA_mobilization_factor = 0.85    # relative mobilization efficiency (LA vs non-LA)

cycles = 20                  # number of lose/regain cycles to simulate


# --- SIMULATION STORAGE ---
fat_mass = [initial_fat]
LA_mass = [initial_LA]
LA_percent = [initial_LA_frac]


# --- RUN MULTI-CYCLE SIMULATION ---
for c in range(cycles):
    F = fat_mass[-1]
    LA = LA_mass[-1]
    nonLA = F - LA

    # --- WEIGHT LOSS PHASE ---
    # Solve proportional mobilization:
    # x = LA burned, y = non-LA burned
    # x + y = cycle_loss
    # (x/LA) : (y/nonLA) = LA_mobilization_factor : 1
    k = cycle_loss / (nonLA + LA * LA_mobilization_factor)
    x = LA * k * LA_mobilization_factor
    y = nonLA * k

    LA_remaining = LA - x
    nonLA_remaining = nonLA - y

    # --- REGAIN PHASE ---
    regained_LA = cycle_regain * LA_regain_frac
    regained_nonLA = cycle_regain - regained_LA

    LA_new = LA_remaining + regained_LA
    nonLA_new = nonLA_remaining + regained_nonLA

    F_new = LA_new + nonLA_new

    fat_mass.append(F_new)
    LA_mass.append(LA_new)
    LA_percent.append(LA_new / F_new)


# --- PLOT ---
plt.figure(figsize=(8,5))
plt.plot(range(cycles+1), LA_percent)
plt.xlabel("Cycle Number")
plt.ylabel("Adipose LA Fraction")
plt.title("Adipose LA% Over Multiple Lose/Regain Cycles (LA-free Regain)")
plt.grid(True)
plt.show()

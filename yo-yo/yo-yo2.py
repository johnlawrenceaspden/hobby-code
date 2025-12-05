#!/usr/bin/env python3

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from datetime import datetime, timedelta

# --------------------------------------------------
# Input Data (your weight readings)
# --------------------------------------------------
keto_water_weight = 1.5
readings = [
    ("2023-05-22", 99),
    ("2023-09-14", 92+keto_water_weight),
    ("2023-10-13", 95.46),
    ("2023-11-11", 91.6+keto_water_weight),
    ("2024-04-05", 97.44),
    ("2024-05-05", 95.50),
    ("2024-08-28", 97.16),
    ("2024-09-22", 94.88+keto_water_weight),
    ("2024-10-06", 96.6),
    ("2024-11-03", 93.4+keto_water_weight),
    ("2024-12-29", 96.6),
    ("2025-01-14", 94.07),
    ("2025-03-28", 96.94),
    ("2025-07-01", 92.37),
    ("2025-09-15", 96.2),
    ("2025-11-23", 91.2+keto_water_weight),
]

dates = [datetime.strptime(d, "%Y-%m-%d") for d,_ in readings]
weights = [w for _,w in readings]

# --------------------------------------------------
# Daily interpolation of weight curve
# --------------------------------------------------
start, end = dates[0], dates[-1]
days = (end - start).days

date_range = [start + timedelta(days=i) for i in range(days+1)]

interp_weights = np.interp(
    [(d - start).days for d in date_range],
    [(d - start).days for d in dates],
    weights
)

# --------------------------------------------------
# Model parameters
# --------------------------------------------------
F0 = 0.30 * interp_weights[0]      # initial body fat mass (30% assumption)
L0 = 0.25 * F0                     # initial LA = 25% of fat
k = np.log(2)/680.0                # LA endogenous turnover (half-life 680 days)

rL = 0.85                          # <<< LA mobilization coefficient
rO = 1.0                           # other fatty acids
p_diet = 0.02                      # dietary LA fraction (2%)

# --------------------------------------------------
# Allocate arrays for fat mass F(t) and LA mass L(t)
# --------------------------------------------------
F = np.zeros(len(interp_weights))
L = np.zeros(len(interp_weights))
F[0], L[0] = F0, L0

# --------------------------------------------------
# Daily update loop (Euler step approximation of the ODE)
# --------------------------------------------------
for i in range(1, len(interp_weights)):
    dW = interp_weights[i] - interp_weights[i-1]
    phi = -dW                      # phi > 0 → fat is being mobilized
    Fi, Li = F[i-1], L[i-1]

    if Fi < 1e-9:
        F[i], L[i] = Fi, Li
        continue

    if phi > 0:  # fat loss
        denom = rO*Fi + (rL - rO)*Li

        if denom <= 0:
            mobil = phi * (Li / max(Fi, 1e-12))
        else:
            mobil = phi * (rL * Li) / denom

        dL = -mobil - k*Li
        dF = -phi

    else:       # fat gain
        gain = -phi
        dL = -k*Li + gain * p_diet
        dF = gain

    F[i] = Fi + dF
    L[i] = max(Li + dL, 0.0)

# --------------------------------------------------
# Build table at original reading dates
# --------------------------------------------------
rows = []
for d, w in readings:
    idx = (datetime.strptime(d, "%Y-%m-%d") - start).days
    rows.append([d, w, F[idx], L[idx], 100 * L[idx] / F[idx]])

df = pd.DataFrame(rows, columns=["date", "weight_kg", "F_kg", "L_kg", "LA_pct"])
print(df)

# --------------------------------------------------
# Plots
# --------------------------------------------------
plt.figure(figsize=(10,5))
plt.plot(date_range, F, label="Fat mass (kg)")
plt.plot(date_range, L, label="LA mass (kg)")
plt.scatter([d for d,_ in readings], [w for _,w in readings], color='red', label='Weight readings')
plt.title("Fat and LA over time (rL=0.85, dietary LA=2%)")
plt.xlabel("Date")
plt.ylabel("Mass (kg)")
plt.legend()
plt.grid()
plt.xticks(rotation=25)
plt.tight_layout()
plt.show()

plt.figure(figsize=(10,4))
plt.plot(date_range, 100 * L / F, label="LA % of fat")
plt.title("LA % of adipose (rL=0.85)")
plt.ylabel("LA (% of fat)")
plt.grid()
plt.xticks(rotation=25)
plt.tight_layout()
plt.show()

# --------------------------------------------------
# Summary statistics
# --------------------------------------------------
final = {
    "initial_fat_kg": F0,
    "initial_LA_kg": L0,
    "final_fat_kg": F[-1],
    "final_LA_kg": L[-1],
    "final_LA_pct": 100 * L[-1]/F[-1]
}
print("\nSummary:\n", final)

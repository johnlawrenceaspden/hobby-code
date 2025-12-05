#!/usr/bin/env python3
"""
Combined script: Program B's ODE LA model + Program A's plotting
with command-line parameter overrides via argparse.
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import argparse
from datetime import datetime, timedelta


# ------------------------------------------------------------
# Utility: merge & sort anthropological lines
# ------------------------------------------------------------
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


# ------------------------------------------------------------
# Anthropological LA% reference lines
# ------------------------------------------------------------
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


# ------------------------------------------------------------
# Command-line argument parser
# ------------------------------------------------------------
def parse_args():
    parser = argparse.ArgumentParser(
        description="Adipose LA depletion model (ODE) + stacked plot visualization."
    )

    parser.add_argument("--initial-bodyfat-frac", type=float, default=0.30)
    parser.add_argument("--initial-LA-frac", type=float, default=0.25)

    parser.add_argument("--rL", type=float, default=0.85,
                        help="LA mobilization factor relative to other FA.")

    parser.add_argument("--p-diet", type=float, default=0.02,
                        help="Dietary LA fraction of fat gain.")

    parser.add_argument("--half-life", type=float, default=680,
                        help="LA endogenous turnover half-life (days).")

    parser.add_argument("--show-table", action="store_true",
                        help="Print summary table for the reading dates.")

    parser.add_argument("--save", action="store_true",
                        help="Save PNG plots instead of displaying them.")

    parser.add_argument("--output-prefix", default="output",
                        help="Prefix for saved plot filenames.")

    return parser.parse_args()


# ------------------------------------------------------------
# Main Program
# ------------------------------------------------------------
def main():
    args = parse_args()

    # -----------------------
    # Input weight readings
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

    dates_readings = [datetime.strptime(d, "%Y-%m-%d") for d, _ in readings]
    weights_readings = [w for _, w in readings]

    # -----------------------
    # Interpolate to daily
    # -----------------------
    start = dates_readings[0]
    end = dates_readings[-1]
    total_days = (end - start).days
    date_range = [start + timedelta(days=i) for i in range(total_days + 1)]

    interp_x = [(d - start).days for d in date_range]
    sample_x = [(d - start).days for d in dates_readings]
    interp_weights = np.interp(interp_x, sample_x, weights_readings)

    # -----------------------
    # Parameters
    # -----------------------
    initial_bodyfat_frac = args.initial_bodyfat_frac
    initial_LA_frac = args.initial_LA_frac
    rL = args.rL
    p_diet = args.p_diet

    k = np.log(2) / args.half_life

    # -----------------------
    # Initial conditions (kg)
    # -----------------------
    n_days = len(interp_weights)

    F = np.zeros(n_days)
    L = np.zeros(n_days)

    F[0] = initial_bodyfat_frac * interp_weights[0]
    L[0] = initial_LA_frac * F[0]

    lean_mass_kg = interp_weights[0] - F[0]

    # -----------------------
    # Daily Euler loop
    # -----------------------
    for i in range(1, n_days):
        dW = interp_weights[i] - interp_weights[i - 1]
        phi = -dW
        Fi, Li = F[i - 1], L[i - 1]

        if Fi <= 0:
            F[i] = Fi
            L[i] = Li
            continue

        if phi > 0:  # fat loss
            denom = Fi + (rL - 1) * Li

            if denom <= 0:
                mobil = phi * (Li / Fi)
            else:
                mobil = phi * (rL * Li) / denom

            dL = -mobil - k * Li
            dF = -phi

        else:  # fat gain
            gain = -phi
            dL = -k * Li + gain * p_diet
            dF = gain

        F[i] = max(Fi + dF, 0)
        L[i] = max(Li + dL, 0)

    # -----------------------
    # Derived quantities
    # -----------------------
    nonLA = F - L
    LA_frac = np.where(F > 0, L / F, 0)

    # -----------------------
    # Summary table
    # -----------------------
    if args.show_table:
        rows = []
        for (d_str, w) in readings:
            idx = (datetime.strptime(d_str, "%Y-%m-%d") - start).days
            rows.append([d_str, w, F[idx], L[idx], 100 * L[idx] / F[idx] if F[idx] > 0 else 0])
        df = pd.DataFrame(rows, columns=["date", "weight_kg", "fat_kg", "LA_kg", "LA_pct"])
        print(df.to_string(index=False))

    # -----------------------
    # Plot: stacked body composition
    # -----------------------
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
    ax1.set_title(f"Body Composition With Adipose LA%  rL={args.rL}, p_diet={args.p_diet}, half-life={args.half_life}")

    ax1.xaxis.set_major_locator(mdates.AutoDateLocator())
    ax1.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m-%d"))
    plt.xticks(rotation=45)
    ax1.grid(True, alpha=0.3)

    # scatter actual weight readings
    ax1.scatter(dates_readings, weights_readings, color="black", s=20, zorder=5)

    # Right axis: LA fraction
    ax2 = ax1.twinx()
    ax2.plot(date_range, LA_frac, color="black", linewidth=2)
    ax2.set_ylabel("Adipose LA Fraction")

    ax2.set_ylim(0, max(0.05, LA_frac.max() * 1.05))

    # Reference lines
    x0 = date_range[0]
    x1 = date_range[-1]
    label_x = x0 + (x1 - x0) * 0.03

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

    fig.tight_layout()

    if args.save:
        fig.savefig(f"{args.output_prefix}_stacked.png", dpi=200)
    else:
        plt.show()



if __name__ == "__main__":
    main()

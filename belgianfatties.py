#!/usr/bin/env python3

m_age = [
    0,
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    18,
    19,
    20,
    25,
    30,
    40,
    50,
    60,
    70,
    80,
]
m_met = [
    0.496,
    0.696,
    0.797,
    0.860,
    0.932,
    0.990,
    1.046,
    1.112,
    1.170,
    1.227,
    1.282,
    1.327,
    1.359,
    1.403,
    1.487,
    1.559,
    1.610,
    1.670,
    1.700,
    1.706,
    1.711,
    1.722,
    1.722,
    1.713,
    1.674,
    1.639,
    1.623,
    1.613,
]
m_kilog = [
    3.20,
    10.00,
    12.00,
    13.21,
    15.07,
    16.70,
    18.04,
    20.16,
    22.26,
    24.09,
    26.12,
    27.85,
    31.00,
    35.32,
    40.50,
    46.41,
    53.39,
    57.40,
    61.26,
    63.32,
    65.00,
    68.29,
    68.90,
    68.81,
    67.45,
    65.50,
    63.03,
    61.22,
]

f_age = [
    0,
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    18,
    20,
    25,
    30,
    40,
    50,
    60,
    70,
    80,
]

f_met = [
    0.483,
    0.690,
    0.780,
    0.850,
    0.910,
    0.974,
    1.032,
    1.096,
    1.139,
    1.200,
    1.248,
    1.275,
    1.327,
    1.386,
    1.447,
    1.475,
    1.500,
    1.544,
    1.562,
    1.570,
    1.577,
    1.579,
    1.555,
    1.536,
    1.516,
    1.514,
    1.506,
]
f_kilog = [
    2.91,
    9.30,
    11.40,
    12.45,
    14.18,
    15.50,
    16.74,
    18.45,
    19.82,
    22.44,
    24.24,
    26.25,
    30.54,
    34.65,
    38.10,
    41.30,
    44.44,
    49.08,
    53.10,
    54.46,
    55.08,
    55.14,
    56.65,
    58.45,
    56.73,
    53.72,
    51.52,
]


m_bmi = [(k / m / m) for k, m in zip(m_kilog, m_met)]

print([(a, round(b, 1)) for b, a in zip(m_bmi, m_age)])

f_bmi = [(k / m / m) for k, m in zip(f_kilog, f_met)]

print([(a, round(b, 1)) for b, a in zip(f_bmi, f_age)])


import pandas as pd

mf = pd.DataFrame(list(zip(m_age, m_bmi)), columns=["m_age", "m_bmi"])
ff = pd.DataFrame(list(zip(f_age, f_bmi)), columns=["f_age", "f_bmi"])

print(mf)
print(ff)


import matplotlib.pyplot as plt

fig, ax = plt.subplots()  # Create the figure and axes object

# Plot the first x and y axes:
mf.plot(x="m_age", y="m_bmi", ax=ax, label="boys", color="blue")
ff.plot(x="f_age", y="f_bmi", ax=ax, label="girls", color="pink")
plt.title("BMI of Rich Belgians before 1830")
plt.show()

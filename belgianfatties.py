age = [
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
met = [
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
kilog = [
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

bmi = [(k / m / m) for k, m in zip(kilog, met)]

print([(a, round(b, 1)) for b, a in zip(bmi, age)])


import pandas as pd

df = pd.DataFrame(list(zip(age, bmi)), columns=["age", "bmi"])


import matplotlib.pyplot as plt

fig, ax = plt.subplots()  # Create the figure and axes object

# Plot the first x and y axes:
df.plot(x="age", y="bmi", ax=ax)

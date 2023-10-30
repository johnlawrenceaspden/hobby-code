#!/usr/bin/env/python3
import math

l = math.pow(1 / 2, 1 / 7)

data = [
    (1, 1),
    (1, 1),
    (1, 1),
    (0, 1),
    (1, 1),
    (0, 1),
    (1, 1),
    (1, 1),
    (1, 1),
    (1, 1),
    (1, 1),
    (1, 1),
]

x = 138 / (1 - l)

for t4, ndt in data:
    x = x * l + t4 * 100 + ndt * 38
    y = x * (1 - l) + ndt * 9
    print(x, y)

print(l)
print(138 / (1 - l))

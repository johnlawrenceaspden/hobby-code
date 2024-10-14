#!/usr/bin/env python3
import random
import matplotlib.pyplot as plt
from collections import Counter


print("hello")

dice = [3] * 200000


def change(n):
    n = n + random.choice([-1, 1])
    if n < 1:
        n = 1
    if n > 6:
        n = 6
    return n


l = []
for i in range(40):
    l.append(sum(dice) / len(dice))
    print(sum(dice) / len(dice), sorted(list(Counter(dice).items())))
    dice = [change(d) for d in dice]


plt.subplot(1, 2, 1)
w = Counter(dice)
plt.bar(w.keys(), w.values())
plt.subplot(1, 2, 2)
w = Counter(dice)
plt.plot(l)
plt.show()

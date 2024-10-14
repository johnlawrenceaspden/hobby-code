#!/usr/bin/env python3
import random
from collections import Counter


print("hello")

dice = [1] * 20


def change(n):
    n = n + random.choice([-1, 1])
    if n < 1:
        n = 1
    if n > 6:
        n = 6
    return n


for i in range(20):
    dice = [change(d) for d in dice]
    print(dice, sum(dice) / len(dice), sorted(list(Counter(dice).items())))

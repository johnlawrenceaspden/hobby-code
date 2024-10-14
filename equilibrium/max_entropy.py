#!/usr/bin/env python3
import random
import math
import matplotlib.pyplot as plt
from collections import Counter


print("hello")


def change(n):
    n = n + random.choice([-1, 1])
    if n < 1:
        n = 1
    if n > 6:
        n = 6
    return n


def shake(dice):
    return [change(d) for d in dice]


def roll(dice):
    return [random.choice([1, 2, 3, 4, 5, 6]) for d in dice]


def exchange(dice):
    for i in range(len(dice) - 1):
        a = random.choice([-1, 1])
        dice[i] = dice[i] + a
        dice[i + 1] = dice[i + 1] - a
    return dice


def exchange_floor(dice):
    for i in range(len(dice) - 1):
        a = random.choice([-1, 1])
        b = dice[i] + a
        c = dice[i + 1] - a
        if b >= 0 and c >= 0:
            dice[i] = b
            dice[i + 1] = c
    return dice


def exchange_floor_no_topology(dice):
    random.shuffle(dice)
    ld = len(dice)
    for i in range(ld - 1):
        # print(dice)
        i1 = i
        i2 = i + 1
        if i1 != i2:
            a = random.choice([-1, 1])
            b = dice[i1] + a
            c = dice[i2] - a
            if b >= 0 and c >= 0:
                dice[i1] = b
                dice[i2] = c
    return dice


def exchange_floor_ceiling_no_topology(dice):
    random.shuffle(dice)
    ld = len(dice)
    for i in range(ld - 1):
        # print(dice)
        i1 = i
        i2 = i + 1
        if i1 != i2:
            a = random.choice([-1, 1])
            b = dice[i1] + a
            c = dice[i2] - a
            if b >= 1 and c >= 1 and b <= 6 and c <= 6:
                dice[i1] = b
                dice[i2] = c
    return dice


def info(p):
    return -p * math.log2(p)


dice = [3] * 20000
avg = []
entropy = []
for i in range(100):
    avg.append(sum(dice) / len(dice))
    c = Counter(dice)
    e = sum([info(x / c.total()) for x in c.values()])
    entropy.append(e)
    if i % 10 == 0:
        print(i, sum(dice) / len(dice), e)
    dice = exchange_floor_ceiling_no_topology(dice)


print("max entropy of six", math.log2(6), "bits")
c = Counter(dice)
print(sum([info(x / c.total()) for x in c.values()]))
p = [b for a, b in sorted(c.items())]
rats = [(a / b) for (a, b) in list(zip(p, p[1:]))]
print(p)
print(rats)

print("Boltzmann distribution over six with average 3")
l = 1.1908
s = sum([1 / (l**i) for i in range(6)])
p = [1 / (l**i) / s for i in range(6)]
e = sum([info(px) for px in p])
a = sum([(p * i) for p, i in zip(p, range(1, 7))])

print(l, s, p, e, a)

print("Boltzmann distribution over six with average 3.5")
l = 1
s = sum([1 / (l**i) for i in range(6)])
p = [1 / (l**i) / s for i in range(6)]
e = sum([info(px) for px in p])
a = sum([(p * i) for p, i in zip(p, range(1, 7))])

print(l, s, p, e, a)

print("Boltzmann distribution over six with average 4")
l = 1 / 1.1908
s = sum([1 / (l**i) for i in range(6)])
p = [1 / (l**i) / s for i in range(6)]
e = sum([info(px) for px in p])
a = sum([(p * i) for p, i in zip(p, range(1, 7))])

print(l, s, p, e, a)

print("Boltzmann distributions over six")
for l in [1 / 1.1908, 1, 1.1908]:
    s = sum([1 / (l**i) for i in range(6)])
    p = [1 / (l**i) / s for i in range(6)]
    e = sum([info(px) for px in p])
    a = sum([(p * i) for p, i in zip(p, range(1, 7))])
    print(l, e, a)


def boltzmann(l, n):
    s = sum([1 / (l**i) for i in range(n)])
    p = [1 / (l**i) / s for i in range(n)]
    e = sum([info(px) for px in p])
    a = sum([(p * i) for p, i in zip(p, range(1, n + 1))])
    return a, e


print(boltzmann(4 / 3, 100))

plt.subplot(1, 4, 1)
plt.bar(c.keys(), c.values())
plt.subplot(1, 4, 2)
plt.plot(rats)
plt.subplot(1, 4, 3)
plt.plot(avg)
plt.subplot(1, 4, 4)
plt.plot(entropy)

plt.show()

#!/usr/bin/env python3

cells = 100

a = [5] * cells
b = ["A"] * cells

print(a)

# a[0]=100

print(a)

import random

# random.seed(1)


def diffuse(a):
    i = random.randrange(0, cells - 1)

    diff = random.choice([-1, 1])
    dir = random.choice([-1, 1])

    if diff == 1:
        if a[i] > 0:
            a[i] = a[i] - diff
            a[i + dir] = a[i + dir] + diff
    elif diff == -1:
        if a[i + dir] > 0:
            a[i] = a[i] - diff
            a[i + dir] = a[i + dir] + diff

    return a

def react(a, b):
    for i in range(cells):
        i = random.randrange(0, cells)
        if b[i]=="A" and a[i]>=5:
            b[i]="B"
            a[i]=a[i]-5
        elif b[i]=="B":
            b[i]="A"
            a[i]=a[i]+5
    return a, b


for j in range(10):
    for i in range(100):
        a = diffuse(a)
        a, b = react(a, b)
    print(a)
    print(b)

print("--------------------")

print(sorted(a))
print(sorted(b))

from collections import Counter

print(sum(a))
print( sorted(list(Counter(a).items())))
print( sorted(list(Counter(b).items())))


#!/usr/bin/env python3
import matplotlib.pyplot as plt
from collections import Counter


cells = 100

a = [50] * cells
b = ["A"] * cells

atobcost=50

print(a)

# a[0]=100

print(b)

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
    i = random.randrange(0, cells)
    if b[i]=="A" and a[i]>=atobcost:
        b[i]="B"
        a[i]=a[i]-atobcost
    elif b[i]=="B":
        b[i]="A"
        a[i]=a[i]+atobcost
    return a, b


for j in range(10):
    for i in range(10):
        a = diffuse(a)
    for i in range(1):
        a, b = react(a, b)
    print(a)
    print(b)
    print( sorted(list(Counter(a).items())))
    print( sorted(list(Counter(b).items())))
    plt.subplot(1,2,1)
    w=Counter(a)
    plt.bar(w.keys(), w.values())
    plt.subplot(1,2,2)
    w=Counter(b)
    plt.bar(w.keys(), w.values())
    plt.show()

    
print("--------------------")

print(sorted(a))
print(sorted(b))



print(sum(a))


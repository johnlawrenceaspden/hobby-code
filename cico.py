#!/usr/bin/env python3
import random
import itertools
import matplotlib.pyplot as plt

print("Hello")

plt.ion()


def normal():
    return random.randint(1, 6) + random.randint(1, 6) - 7


def cicoit(w):
    while True:
        w = w + 0.001 * normal()
        yield w


def homeoit(w):
    stable = w
    while True:
        w = w + 0.001 * normal()
        w = w + 0.001 * (stable - w)
        yield w


def homeoit_change(w):
    stable = w
    while True:
        w = w + 0.001 * normal()
        w = w + 0.001 * (stable - w)
        val = yield w
        if val is not None:
            print("stable is now", val)
            stable = val


cico = list(itertools.islice(cicoit(100), 5000))
plt.plot(cico, label="CICO")


homeo = list(itertools.islice(homeoit(100), 5000))
plt.plot(homeo, label="homeo")

homeoit_c = homeoit_change(100)

homeo_c = list(itertools.islice(homeoit_c, 500))
homeoit_c.send(99)
homeo_c = homeo_c + list(itertools.islice(homeoit_c, 300))
homeoit_c.send(100)
homeo_c = homeo_c + list(itertools.islice(homeoit_c, 5000))


plt.plot(homeo_c, label="homeo_c")


plt.legend()
plt.show()

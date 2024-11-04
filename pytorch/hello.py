#!/usr/bin/env python3

import torch

print(torch.cuda.is_available() and "yay, we have cuda" or "bugger, no cuda")


def p(x):
    print(x, type(x))

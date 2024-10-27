#!/usr/bin/env python
# in emacs use the virtual envs menu to activate the python venv

# https://pytorch.org/tutorials/beginner/basics/tensorqs_tutorial.html
# https://pytorch.org/get-started/locally/

# sudo apt update
# sudo apt install python3.11-venv
# python3 -m venv .
# source ./bin/activate
# pip3 install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cu118
# python
# >>> import torch
# >>> torch.cuda.is_available()
# False


import torch


def p(x):
    print(x, type(x))


print(torch.cuda.is_available() and "yay" or "bugger")

import numpy as np

data = [[1, 2], [3, 4]]

print(data)

x_data = torch.tensor(data)

print(x_data)

np_data = np.array(data)

print(np_data)

print([type(x) for x in [data, x_data, np_data]])

x_ones = torch.ones_like(x_data)

print(x_ones)

x_rand = torch.rand_like(x_data, dtype=torch.float)

p(x_rand)

shape = (2, 3)
rand_tensor = torch.rand(shape)
p(rand_tensor)

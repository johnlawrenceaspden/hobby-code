#!/usr/bin/env python
# in emacs use the virtual envs menu to activate the python venv
import torch

print(torch.cuda.is_available() and "yay" or "bugger")

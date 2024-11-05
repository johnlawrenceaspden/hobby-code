#!/usr/bin/env python3

# source ./bin/activate
# pip install validators matplotlib torch requests

import torch
import validators
import matplotlib

# ModuleNotFoundError: No module named 'torch'
# torch is installed in a venv rather than system-wide

# in terminal
# source ./bin/activate

# in emacs
# (progn (pyvenv-activate "~/hobby-code/pytorch") (pyvenv-restart-python))
# C-x C-e at the end of the line to evaluate this as lisp

# (pyvenv-activate "~/hobby-code/pytorch")
# bound to elpy menu virtual envs/activate but you have to look for the directory by hand
# or it seems to pick up the venv if you do e hello.py from a directory with the venv already activated

# and then restart the python interpreter virtual envs/restart-python-process
# (pyvenv-restart-python)

print(torch.cuda.is_available() and "yay, we have cuda" or "bugger, no cuda")


def p(x):
    print(x, type(x))


# ----------------


import torch
from PIL import Image
import torchvision.transforms as transforms
import numpy as np
import json
import requests
import matplotlib.pyplot as plt
import warnings

warnings.filterwarnings("ignore")

# This is an IPython magic command to display in a notebook
# %matplotlib inline

device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu")
print(f"Using {device} for inference")

resnet50 = torch.hub.load(
    "NVIDIA/DeepLearningExamples:torchhub", "nvidia_resnet50", pretrained=True
)
utils = torch.hub.load(
    "NVIDIA/DeepLearningExamples:torchhub", "nvidia_convnets_processing_utils"
)

resnet50.eval().to(device)

uris = [
    "http://images.cocodataset.org/test-stuff2017/000000024309.jpg",
    "http://images.cocodataset.org/test-stuff2017/000000028117.jpg",
    "http://images.cocodataset.org/test-stuff2017/000000006149.jpg",
    "http://images.cocodataset.org/test-stuff2017/000000004954.jpg",
]

batch = torch.cat([utils.prepare_input_from_uri(uri) for uri in uris]).to(device)

with torch.no_grad():
    output = torch.nn.functional.softmax(resnet50(batch), dim=1)

results = utils.pick_n_best(predictions=output, n=5)

for uri, result in zip(uris, results):
    img = Image.open(requests.get(uri, stream=True).raw)
    img.thumbnail((256, 256), Image.LANCZOS)
    plt.imshow(img)
    plt.show()
    print(result)

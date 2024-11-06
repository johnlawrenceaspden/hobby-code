#!/usr/bin/env python3

# source ./bin/activate
# pip install validators matplotlib torch requests
#

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
import cv2
import os

print(os.getcwd())
os.chdir("/home/john/hobby-code/pytorch")
print(os.getcwd())

# Read the original image
# rotate 20 degrees clockwise like so to line up ridge with horizontal
# convert -rotate 20 1nealclose.png 1nealclose+20.png

img = cv2.imread("1nealclose+20.png")
# # Convert to grayscale
# img_gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

img_gray = cv2.imread("1nealclose+20.png", cv2.IMREAD_GRAYSCALE)

# Display original image
cv2.imshow("Original", img)
# cv2.imshow("Original Gray", img_gray)


# # Blur the image for better edge detection
img_blur = cv2.GaussianBlur(img_gray, (3, 3), 0)
# img_blur = img_gray

# cv2.imshow("Original Blur", img_blur)

# Sobel Edge Detection
sobelx = cv2.Sobel(
    src=img_blur, ddepth=cv2.CV_64F, dx=1, dy=0, ksize=5
)  # Sobel Edge Detection on the X axis
cv2.imshow("Sobel X", sobelx)

sobely = cv2.Sobel(
    src=img_blur, ddepth=cv2.CV_64F, dx=0, dy=1, ksize=5
)  # Sobel Edge Detection on the Y axis
cv2.imshow("Sobel Y", sobely)


# Canny Edge Detection
edges = cv2.Canny(
    image=img_blur, threshold1=100, threshold2=200
)  # Canny Edge Detection
cv2.imshow("Canny Edge Detection", edges)


while True:
    res = cv2.waitKey(0)
    print(
        "Waiting for ESC (27): You pressed %d (0x%x), LSB: %d (%s)"
        % (res, res, res % 256, repr(chr(res % 256)) if res % 256 < 128 else "?")
    )
    if res == 27:
        break

cv2.destroyAllWindows()

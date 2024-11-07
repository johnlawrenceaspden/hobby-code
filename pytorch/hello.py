#!/usr/bin/env python3

# source ./bin/activate
# pip install validators matplotlib torch requests
#

import torch
import validators
import matplotlib
import cv2
import os
import numpy as np
import copy
import math

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

TAU = np.pi * 2
DEGREE = TAU / 360
TOL = 3 * DEGREE

print(os.getcwd())
os.chdir("/home/john/hobby-code/pytorch")
print(os.getcwd())

# Read the original image
# rotate 20 degrees clockwise like so to line up ridge with horizontal
# convert -rotate 20 1nealclose.png 1nealclose+20.png

img = cv2.imread("1nealclose+20.png")
# # Convert to grayscale
img_gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

# or this also works
# img_gray = cv2.imread("1nealclose+20.png", cv2.IMREAD_GRAYSCALE)

# Display original image
cv2.imshow("Original", img)
# cv2.imshow("Original Gray", img_gray)


# # Blur the image for better edge detection
img_blur = cv2.GaussianBlur(img_gray, (3, 3), 0)
# img_blur = img_gray

cv2.imshow("Original Blur", img_blur)

# Sobel Edge Detection
sobelx = cv2.Sobel(
    src=img_blur, ddepth=cv2.CV_64F, dx=1, dy=0, ksize=5
)  # Sobel Edge Detection on the X axis
# cv2.imshow("Sobel X", sobelx)

sobely = cv2.Sobel(
    src=img_blur, ddepth=cv2.CV_64F, dx=0, dy=1, ksize=5
)  # Sobel Edge Detection on the Y axis
# cv2.imshow("Sobel Y", sobely)


# Canny Edge Detection
edges = cv2.Canny(
    image=img_blur, threshold1=100, threshold2=200
)  # Canny Edge Detection
cv2.imshow("Canny Edge Detection", edges)

# GPT's version
# Also works but there's more noise
gpt_edges = cv2.Canny(img_blur, 50, 150, apertureSize=3)
cv2.imshow("Canny Edge Detection GPT", gpt_edges)

# https://chatgpt.com/
# how would I find straight lines in an image in opencv2


lines = cv2.HoughLines(gpt_edges, 1, (TAU / 2) / 180, 150)

hough_lines_img = copy.deepcopy(img)


def quasiparallel(theta, phi, tolerance):
    return abs(theta - phi) < tolerance or abs(theta - TAU - phi) < tolerance


if lines is not None:
    for rho, theta in lines[:, 0]:
        # Calculate the line's start and end points
        a = np.cos(theta)
        b = np.sin(theta)
        x0 = a * rho
        y0 = b * rho
        x1 = int(x0 + 1000 * (-b))
        y1 = int(y0 + 1000 * (a))
        x2 = int(x0 - 1000 * (-b))
        y2 = int(y0 - 1000 * (a))

        # Check if the line is horizontal or vertical
        if quasiparallel(theta, TAU / 4, TOL) or quasiparallel(theta, 0, TOL):
            # Draw the line on the original image
            cv2.line(hough_lines_img, (x1, y1), (x2, y2), (0, 0, 255), 2)


cv2.imshow("Hough Lines", hough_lines_img)


hough_lines_p = cv2.HoughLinesP(
    gpt_edges, 1, (TAU / 2) / 180, threshold=50, minLineLength=10, maxLineGap=10
)


hough_lines_p_img = copy.deepcopy(img)
if hough_lines_p is not None:
    for x1, y1, x2, y2 in hough_lines_p[:, 0]:
        theta = math.atan2((y2 - y1), (x2 - x1))
        if quasiparallel(theta, TAU / 4, TOL) or quasiparallel(theta, 0, TOL):
            cv2.line(hough_lines_p_img, (x1, y1), (x2, y2), (0, 0, 255), 2)

cv2.imshow("Hough Lines P", hough_lines_p_img)


while True:
    res = cv2.waitKey(0)
    print(
        "Waiting for ESC (27): You pressed %d (0x%x), LSB: %d (%s)"
        % (res, res, res % 256, repr(chr(res % 256)) if res % 256 < 128 else "?")
    )
    if res == 27:
        break

cv2.destroyAllWindows()

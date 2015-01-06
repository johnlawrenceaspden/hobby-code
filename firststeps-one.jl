#!/usr/bin/julia

# Here are my notes from playing with the code at the start of the Kaggle Julia tutorial

# You only need to do this once for a given Julia installation, so I've commented it
# out so it doesn't get done every time
# Pkg.add("Images")
# Pkg.add("DataFrames")

# Paths to our data files
labelsfile="./trainLabels.csv"
imagedirectory="./trainResized/"


# The ground truth for our dataset is in a csv file, which we can read
# in using the DataFrames library
using DataFrames

# formatted print is a macro, hence the @ sign, it's much like C's printf
@printf("Reading %s\n", labelsfile)
labels=readtable(labelsfile)
@printf("Read in %d labels from %s\n", size(labels)[2], labelsfile)
@printf("Image %i is of an %s\n",labels[1,1],labels[1,2])


# indexing is like matlab, all of rows 1 to 3
labels[1:3,:]
## 3x2 DataFrame
## | Row | ID | Class |
## |-----|----|-------|
## | 1   | 1  | "n"   |
## | 2   | 2  | "8"   |
## | 3   | 3  | "T"   |

# 

using Images
img=imread("$(imagedirectory)1.Bmp")

# The kaggle julia tutorial then tells us to do:
temp=float32(img)
## RGB Image with:
##   data: 20x20 Array{RGB{Float32},2}
##   properties:
##     IMcs: sRGB
##     spatialorder:  x y
##     pixelspacing:  1 1

## What did that just do?

help(float32)
## Base.float32(x)
##    Convert a number or array to "Float32" data type

# Not enormously the wiser now...

# Our image is 2 dimensional
ndims(temp)
#2

# And 20x20
size(temp)
#(20,20)

# There's something funny going on here. This first image is a 20x20 colour bitmap
# and yet the tutorial on the website has something about converting images to greyscale if ndims is 3. Our ndims is 2.

# finally we want to convert our 20x20 image to a 1x400 vector
imageSize=400
flat=reshape(temp,1,imageSize)




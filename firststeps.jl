#!/usr/bin/julia

# Our next task is to read in all the training images and make them into a big matrix

using DataFrames
using Images

# Paths to our data files
labelsfile="./trainLabels.csv"
imagedirectory="./trainResized/"

@printf("Reading %s\n", labelsfile)
labels=readtable(labelsfile)
@printf("Read in %d labels from %s\n", size(labels)[2], labelsfile)
@printf("Image %i is of an %s\n",labels[1,1],labels[1,2])

no_of_images=size(labels)[1]
@printf("reading %s images\n", no_of_images)

imageSize=20*20

#Create a gigantic array to put the images in
x=zeros(no_of_images,imageSize)

# We can iterate over a dataframe's columns by name
for (a,b) in enumerate(labels[:ID]);
    image="$(imagedirectory)$(b).Bmp"
    img=imread(image)
    temp=float32(img)
    assert(ndims(temp)==2)
    assert(size(temp)==(20,20))
    @printf("%s %s\n",a,image)
    x[a,:]=reshape(temp,1,imageSize)
end


image="$(imagedirectory)$(1).Bmp"
img=imread(image)
img_gs=convert(Image{Gray},img)
img_uint8=reinterpret(Uint8,data(img_gs))

# The kaggle julia tutorial then tells us to do:
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

#2

# And 20x20

#(20,20)

# There's something funny going on here. This first image is a 20x20 colour bitmap
# and yet the tutorial on the website has something about converting images to greyscale if ndims is 3. Our ndims is 2.

# finally we want to convert our 20x20 image to a 1x400 vector
imageSize=400
flat=reshape(temp,1,imageSize)




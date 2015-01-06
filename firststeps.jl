#!/usr/bin/julia

# Our next task is to read in all the training images and make them into a big matrix

using DataFrames
using Images # lots of warnings, but it's ok

# Paths to our data files
labelsfile="./trainLabels.csv"
imagedirectory="./trainResized/"

@printf("Reading %s\n", labelsfile)
labels=readtable(labelsfile)
no_of_images=size(labels)[1]
@printf("Read in %d labels from %s\n", no_of_images, labelsfile)
@printf("Image %i is of an %s\n",labels[1,1],labels[1,2])


@printf("reading %s images\n", no_of_images)

imageSize=20*20


# The code on the kaggle website doesn't seem to work for me, but after a bit of hunting around, this is how to get the desired effect on the first image:
image="$(imagedirectory)1.Bmp"
img=imread(image)
# turn our colour image into a greyscale image
img_gs=convert(Image{Gray},img)
# turn the specialized image format into an array of floats
img_floats=reinterpret(Float32,float32(img_gs))
# turn the 20x20 array into a 1x400 vector
img_vec=reshape(img_floats,1,imageSize)

# After all that, I feel the need to check I haven't buggered it up
Pkg.add("ImageView")
require("ImageView")
# Should pop up a little grey 'n'
ImageView.view(grayim(reshape(img_vec,20,20)))

# So now we want to use that process to convert all the images into
# one big array of image vectors
#Create a gigantic array to put the images in
x=zeros(no_of_images,imageSize)

# We can iterate over a dataframe's columns by name (it takes a while!)
for (a,b) in enumerate(labels[:ID]);
    image="$(imagedirectory)$(b).Bmp"
    img=imread(image)
    assert(size(img)==(20,20)) # paranoia
    img_gs=convert(Image{Gray},img)
    assert(size(img_gs)==(20,20))
    img_floats=reinterpret(Float32,float32(img_gs))
    assert(size(img_floats)==(20,20))
    img_vec=reshape(img_floats,1,imageSize)
    assert(size(img_vec)==(1,400))

    @printf("%s %s\n",a,image)
    x[a,:]=img_vec
end



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




#!/usr/bin/julia

# Right, let's see if we can actually get Julia to do a bit of Machine Learning:

# Make sure we've got the necessary libraries installed
Pkg.add("DataFrames")
Pkg.add("Images")
Pkg.add("ImageView")
Pkg.add("DecisionTree")

# And loaded
require("DataFrames")
require("Images")
require("ImageView")
require("DecisionTree")

# Now let's load our training data
labelsfile="./trainLabels.csv"
imagedirectory="./trainResized/"
imageSize=20*20

# load the ground truth labels
labels=DataFrames.readtable(labelsfile)
no_of_images=size(labels)[1]
@printf("Read in %d labels from %s\n", no_of_images, labelsfile)

# Create a gigantic array of training images
# This is hella slow. I was promised speed!
train=zeros(no_of_images,imageSize)
for (a,b) in enumerate(labels[:ID]);
    image="$(imagedirectory)$(b).Bmp"
    img=Images.imread(image)
    assert(size(img)==(20,20)) # paranoia
    img_gs=convert(Images.Image{Images.Gray},img)
    assert(size(img_gs)==(20,20))
    img_floats=reinterpret(Float32,float32(img_gs))
    assert(size(img_floats)==(20,20))
    img_vec=reshape(img_floats,1,imageSize)
    assert(size(img_vec)==(1,400))

    @printf("%s %s\n",a,image)
    train[a,:]=img_vec
end

# We now need to make the ground truth labels
#
# We can make little functions
funct=(x -> int(x[1]))
funct("A") # 65
# And map them over things
nearlywhatwewant=map(funct, labels[:Class])
# Unfortunately this doesn't appear to be good enough
# This is a DataArray, whatever that means, and we want an Array, whatever that is
# And this seems to do the conversion, although God knows why:
trainlabels=int(nearlywhatwewant)

# Let's just check that we're still sane
char(trainlabels[20]) # should be a k
ImageView.view(Images.grayim(reshape(train[20,:],20,20))) # should be a picture of a k




# All this preparation having been done, we can now feed the data into
# a random-forest making function:
sherwood=DecisionTree.build_forest(trainlabels,train,20,50,1.0) #again, bloody ages

## Ensemble of Decision Trees
## Trees:      50
## Avg Leaves: 2208.14
## Avg Depth:  19.3

# Now, how well does this forest do on the data on which it was trained?

shouldbegood=DecisionTree.apply_forest(sherwood, train)

# Looks like it only got one wrong
wrong=find(shouldbegood.!=trainlabels) # 3055

@printf("Testing a Random Forest on the data used to train it: errors=%s", size(wrong)[1])

char(shouldbegood[3055]) #E
char(trainlabels[3055])  #1

# Apparently this 1 looks more like an E

#Sure looks like a 1 to me!
ImageView.view(Images.grayim(reshape(train[3055,:],20,20)))

# But you can't fault the classifier on the other 6282 images.


















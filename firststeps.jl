#!/usr/bin/julia

# Real men run their classifiers on data that hasn't been used to train the classifier in the first place.

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

# Where the data is (Kaggle provide two zip files that unpack into these directories)
trainlabelsfile="./trainLabels.csv"
trainimagedirectory="./trainResized/"

testlabelsfile="./sampleSubmission.csv"
testimagedirectory="./testResized/"

# All the resized images are 20x20
imageSize=20*20

# Get the data sets and turn them into an index of file numbers and a
# large array of image data where each image is reduced to a greyscale
# 1x400 vector. This is hella slow. I was promised speed!
function readimages(csvfile, imagedirectory)

    #read the csv file
    labels=DataFrames.readtable(csvfile)
    no_of_images=size(labels)[1]
    @printf("Read in %d labels from %s\n", no_of_images, csvfile)

    # read the images, grey them, flatten them, and put them in an array
    x=zeros(no_of_images,imageSize)
    for (a,b) in enumerate(labels[:ID]);
        image="$(imagedirectory)$(b).Bmp"
        img=Images.imread(image)
        img_gs=convert(Images.Image{Images.Gray},img)
        img_floats=reinterpret(Float32,float32(img_gs))
        img_vec=reshape(img_floats,1,imageSize)
        @printf("%s %s\n",a,image)
        x[a,:]=img_vec
    end
    
    return labels,x
end;

trainlabels,trainimages=readimages(trainlabelsfile, trainimagedirectory)

# Our classifier can't deal with non-numeric class labels
# So to use it we convert the ground truth labels like "A" into numbers like 65
# 1963 did phone, but I forgot to warn them
trainlabelsbodge=int(map((x -> int(x[1])), trainlabels[:Class]))

# Let's just check that we're still sane
char(trainlabelsbodge[20]) # should be a k
ImageView.view(Images.grayim(reshape(trainimages[20,:],20,20))) # should be a picture of a k

# All this preparation having been done, we can now feed the data into
# a random-forest making function:
sherwood=DecisionTree.build_forest(trainlabelsbodge,trainimages,20,50,1.0) #again, bloody ages

## Ensemble of Decision Trees
## Trees:      50
## Avg Leaves: 2208.14
## Avg Depth:  19.3

# Now, how well does this forest do on the data on which it was trained?

@printf("Training Random Forest\n")
shouldbegoodbodge=DecisionTree.apply_forest(sherwood, trainimages)

shouldbegood=map((x->string(char(x))),shouldbegoodbodge)


# On its training data, it only got one wrong
wrong=find(shouldbegood.!=trainlabels[:Class]) # 3055

@printf("Testing a Random Forest on the data used to train it: errors=%s\n", size(wrong)[1])

# We can also try it on the test data

@printf("Reading in the test images\n")
testlabels,testimages=readimages(testlabelsfile, testimagedirectory)

@printf("Classifying the test images\n")
doesitworkbodge=DecisionTree.apply_forest(sherwood, testimages)
doesitwork=map((x->string(char(x))),doesitworkbodge)

# Rather embarrassingly, I can't tell what this is
ImageView.view(Images.grayim(reshape(testimages[1,:],20,20)))
# The classifier's thinks it's an H, which is reasonable
doesitwork[1] #"H"

# E, for defs
ImageView.view(Images.grayim(reshape(testimages[2,:],20,20)))
doesitwork[2] #"E"

# Christ on a bike
ImageView.view(Images.grayim(reshape(testimages[3,:],20,20)))
doesitwork[3] #"7"

# This is a P on its side. They're cheating!
ImageView.view(Images.grayim(reshape(testimages[4,:],20,20)))
doesitwork[4] #"O"

# Anyhow, we can replace the dummy labels in our test labels file (I know, I know..)
testlabels[:Class]=doesitwork

# And write it back out for submission to Kaggle
DataFrames.writetable("doom.csv",testlabels)

# Kaggle learn me that I've scored 44%, and am now 26th out of 39 in the competition.

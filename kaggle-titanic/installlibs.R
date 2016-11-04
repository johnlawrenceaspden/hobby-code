# install required R packages

## sudo vi /etc/apt/sources.list
## deb http://cran.ma.imperial.ac.uk/bin/linux/debian jessie-cran3/

## sudo apt-key adv --keyserver keys.gnupg.net --recv-key 6212B7B7931C4BB16280BA1306F90DE5381BA480

## sudo apt update
## sudo apt upgrade
## sudo apt dist-upgrade

## find all packages depending on r-base-core, which is most r packages
## apt-cache rdepends r-base-core


# sudo apt install r-cran-ggplot2 r-cran-scales r-cran-randomforest r-cran-rgtk2

## Now we need a way of installing things that aren't already installed
## http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

list.of.packages=c('earth',
'mlbench',
'caret',
'pROC',
'ggthemes',
'mice',
'VIM',
'randomForest',
'rpart.plot',
'RColorBrewer',
'rattle',
'party',
'dplyr',
'mice',
'ggplot2')


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


## Hand version

## install.packages("earth")
## install.packages('ggthemes')
## install.packages('mice')
## install.packages('VIM')
## install.packages('randomForest')
## install.packages('rpart.plot')
## install.packages('RColorBrewer')
## install.packages('mice')

## install.packages('rattle')
## install.packages('party')
## install.packages('dplyr')
## install.packages("mlbench")
## install.packages("caret")
## install.packages("pROC")




library("earth")
library("mlbench")
library("caret")
library("pROC")
library('ggthemes')
library('mice')
library('VIM')
library('randomForest')
library('rpart.plot')
library('RColorBrewer')
library('rattle')
library('party')
library('dplyr')
library('mice')



## how do these sparse matrices work anyway?
## https://stackoverflow.com/questions/12029177/sparse-matrix-to-a-data-frame-in-r#12029502


## Suppose we've got a weighted graph A-1->X, A-4->Z, C-2->X, C-3->Y (insert graphviz picture)

## We could represent it as a standard R matrix

rmat <- matrix(data = c(1, 0, 2, 0, 0, 3, 4, 0, 0), nrow = 3, ncol = 3,
              dimnames = list(Origin      = c("A", "B", "C"),
                              Destination = c("X", "Y", "Z")))
rmat
##       Destination
## Origin X Y Z
##      A 1 0 4
##      B 0 0 0
##      C 2 3 0
class(rmat)
## [1] "matrix"
summary(rmat)
##        X             Y             Z        
##  Min.   :0.0   Min.   :0.0   Min.   :0.000  
##  1st Qu.:0.5   1st Qu.:0.0   1st Qu.:0.000  
##  Median :1.0   Median :0.0   Median :0.000  
##  Mean   :1.0   Mean   :1.0   Mean   :1.333  
##  3rd Qu.:1.5   3rd Qu.:1.5   3rd Qu.:2.000  
##  Max.   :2.0   Max.   :3.0   Max.   :4.000

dfrmat<-as.data.frame(rmat)
dfrmat
##   X Y Z
## A 1 0 4
## B 0 0 0
## C 2 3 0

## Notice that we've lost the labels Origin and Destination




## Or as a matrix from the Matrix package ( a "dgeMatrix" )
require(Matrix)
mat <- Matrix(data = c(1, 0, 2, 0, 0, 3, 4, 0, 0), nrow = 3, ncol = 3,
              dimnames = list(Origin      = c("A", "B", "C"),
                              Destination = c("X", "Y", "Z")),
              sparse = FALSE)
mat
## 3 x 3 Matrix of class "dgeMatrix"
##       Destination
## Origin X Y Z
##      A 1 0 4
##      B 0 0 0
##      C 2 3 0
class (mat)
## [1] "dgeMatrix"
## attr(,"package")
## [1] "Matrix"
summary (mat)
##    Length     Class      Mode 
##         9 dgeMatrix        S4 

rownames(mat)
## [1] "A" "B" "C"
colnames(mat)
## [1] "X" "Y" "Z"

## We can coerce into an ordinary matrix 
as.matrix(mat)
##       Destination
## Origin X Y Z
##      A 1 0 4
##      B 0 0 0
##      C 2 3 0

## And from there to data frame, again losing the labels
as.data.frame(as.matrix(mat))
  X Y Z
A 1 0 4
B 0 0 0
C 2 3 0



## Or we can make a sparse matrix
smat <- Matrix(data = c(1, 0, 2, 0, 0, 3, 4, 0, 0), nrow = 3, ncol = 3,
              dimnames = list(Origin      = c("A", "B", "C"),
                              Destination = c("X", "Y", "Z")),
              sparse = TRUE)
smat
## 3 x 3 sparse Matrix of class "dgCMatrix"
##       Destination
## Origin X Y Z
##      A 1 . 4
##      B . . .
##      C 2 3 .

## Notice the dots instead of zeros

rownames(smat) # "A" "B" "C"
colnames(smat) # "X" "Y" "Z"

## Summary is really different here
summ <- summary(smat)
summ

## And has subvectors
summ$i
summ$j
summ$x

## Which can be used to reorder the column names
rownames(mat)[summ$i]
colnames(mat)[summ$j]

## Giving us a different way to turn it into a data frame
df<- data.frame(Origin      = rownames(mat)[summ$i],
                Destination = colnames(mat)[summ$j],
                Weight      = summ$x)
df


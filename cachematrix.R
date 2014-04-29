########################################################################
# general notes
########################################################################
# The following functions, makeCacheMatrix and cacheSolve,
# are designed to cache the inverse of a matrix so it only 
# needs to be calculated the first time it is needed.
#
# How these functions should be used:
#
# 1.  Pass a matrix of interest to the makeCacheMatrix function
#     and store the result in another variable.  
#
#     Note:     A convenient notation might be to give the new variable 
#               the same name as the matrix with ".obj" appended.
#
#     Example:  # linear regression:  assume that design matrix X and
#               # response vector y have been defined.
#               XtX <- t(X) %*% X
#               XtX.obj <- makeCacheMatrix(XtX)
#
# 2.  Use cacheSolve instead of solve whenever the inverse of the 
#     matrix is needed.
#
#     Example:  beta.hat <- cacheSolve(XtX.obj) %*% t(X) %*% y
#               H <- X %*% cacheSolve(XtX.obj) %*% t(X)
#
# 3.  If the matrix needs to be reset, the set.matrix() accessor 
#     function can be used to avoid having to create a new object.
#
#     Example:  # after recalculating XtX
#               XtX.obj$set.matrix(XtX)


########################################################################
# makeCacheMatrix function
########################################################################
# purpose:  this function will create a CacheMatrix "object"
# (actually a list of functions that share a parent environment,
# not a true class, though it could easily be made into an
# S3 or S4 class).
# 
# input: a matrix
# 
# output: a list consisting of four accessor functions defined 
# within the parent environment:
#
# 1.  set.matrix
# 2.  get.matrix
# 3.  set.inverse
# 4.  get.inverse
# 
# OOP Note:
# The matrix and its inverse are stored in the environment associated
# with the makeCacheMatrix function.  Since these variables are not
# accessible outside this function, they serve as private data members.
# Similarly, the accessor functions serve as public methods.
# 
# author:  Jason Thiese
# date created:  4/27/2014
# last modified:  4/27/2014
# version:  1.0
makeCacheMatrix <- function(mat = matrix()) {
  mat.inverse <- NULL 
  
  # At this point, two variables exist in the environment
  # associated with makeCacheMatrix.
  #
  # 1.  mat, the matrix of interest.
  # 2.  mat.inverse, the inverse of mat, which will not be
  #     computed until the first time it is requested.
  #
  # The following functions will get and set these variables.
  
  # set mat
  set.matrix <- function(new.matrix) {
    mat <<- new.matrix   
    # After setting a new matrix, the inverse will need to be recomputed.
    mat.inverse <<- NULL
  }
  
  # get mat
  get.matrix <- function() { 
    return(mat)
  }
  
  # set mat.inverse
  set.inverse <- function(m.inv) {
    mat.inverse <<- m.inv 
  }
  
  # get mat.inverse
  get.inverse <- function() {
    return(mat.inverse)
  }
  
  # return a list containing these four functions.
  return(list(set.matrix = set.matrix, get.matrix = get.matrix, 
              set.inverse = set.inverse, get.inverse = get.inverse))
}


########################################################################
# cacheSolve function
########################################################################
# purpose:  this function will get the inverse of a matrix.  It will
#           perform the inverse computation only if the inverse has 
#           not been cached.
# 
# input: a CacheMatrix "object"
# 
# output: the inverse of the matrix wrapped in the CacheMatrix "object"
# 
# author:  Jason Thiese
# date created:  4/27/2014
# last modified:  4/27/2014
# version:  1.0
cacheSolve <- function(CacheMatrix.object, ...) {
  inv <- CacheMatrix.object$get.inverse() 
  
  # If the inverse has already been calculated, return it.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse and store it in the environment
  # associated with makeCacheMatrix.object.
  matx <- CacheMatrix.object$get.matrix()
  inv <- solve(matx, ...)
  CacheMatrix.object$set.inverse(inv)
  
  return(inv)
}


########################################################################
# TEST
########################################################################
# general functionality
test.obj <- makeCacheMatrix(matrix(c(1,2,2,5), nrow = 2))
test.obj$get.matrix()
test.obj$get.inverse() # should be NULL
cacheSolve(test.obj)   # no message
cacheSolve(test.obj)   # message:  getting cached data
test.obj$set.matrix(matrix(c(3,1,5,17), nrow=2))
test.obj$get.matrix()  # new matrix displayed

# linear regression example
# get an appropriate dataset
library(datasets)
data(women)
# create the design matrix
X <- cbind(rep(1, nrow(women)), women$height)
# set the response vector
y <- women$weight
# create a matrix that will be used in multiple computations
XtX <- t(X) %*% X
# create a CacheMatrix wrapper for the matrix
XtX.obj <- makeCacheMatrix(XtX)
# estimate model parameters
beta.hat <- cacheSolve(XtX.obj) %*% t(X) %*% y
# reuse the computed inverse to compute the hat matrix
H <- X %*% cacheSolve(XtX.obj) %*% t(X)
# compare beta.hat with the parameter estimates produced by lm
round(beta.hat, 2)
lm(weight ~ height, data = women)

# clean up the workspace
rm(list=c("X", "XtX", "XtX.obj", "y", "H", "beta.hat", "women",
          "test.obj", "cacheSolve", "makeCacheMatrix"))

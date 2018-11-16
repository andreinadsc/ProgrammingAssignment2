## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
## We have the variables matrix (a square invertible matrix) and inverse (stored the inverse of matrix)
## return: a list containing this functions:
##    1. set: set the value of the matrix.
##    2. get: get the value of the matrix.
##    3. setInverse: set the inverse of the matrix.
##    4. getInverse: get the inverse of the matrix.


makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  get <- function() matrix
  setInverse <- function() inverse <<- solve(matrix)
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve return the inverse of the special "matrix" returned by makeCacheMatrix()
## Takes a matrix (output of makeCacheMatrix()) and check f the inverse has already been calculated
## if it was, return the inverse stored in cache
## if not, calculates the inverse with solve function and set this inverse value via setInverse function
## return the inverse of the  matrix
cacheSolve <- function(matrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- matrix$getInverse()
  if(!is.null(inverse)) {
    return(inverse)
  }
  data <- matrix$get()
  return(data)
  inverse.new <- solve(data)
  matrix$setInverse(inverse.new)
  return(inverse.new)
}
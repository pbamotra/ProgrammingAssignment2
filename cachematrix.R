## Author 	: 	Pankesh Bamotra
## Date		:	April 18, 2014

## Matrix inversion is usually a costly computation 
## and there may be some benefit in caching the inverse 
## of a matrix rather than compute it repeatedly. This 
## R script has a pair of functions that caches the 
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # This function creates a special "matrix" object that 
  # can cache its inverse.
  #
  # Args:
  #   x: the input matrix whose inverse needs to be cached
  #
  # Returns:
  #   A special "matrix", which is really a list containing a 
  #   function to :-
  #	  1. set the value of the matrix
  #   2. get the value of the vector
  #   3. set the value of the mean
  #   4. get the value of the mean
  
  # initialize the cached inverse value to NULL
    inverse <- NULL

  # to set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
  # to get the value of the matrix
    get <- function() x
  # to set the inverse
    setinv <- function(_inverse) inverse <<- _inverse
  # to get the inverse
    getinv <- function() inverse

  # return a list of all the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {

  # This function calculates the inverse of the special 
  # "matrix" created with the above function. However, it first 
  # checks to see if the inverse has already been calculated. 
  # If so, it gets the inverse from the cache and skips the 
  # computation. Otherwise, it calculates the inverse of the 
  # matrix and sets the value of the inverse in the cache via 
  # the setinv function.
  #
  # Args:
  #   x: the input matrix whose inverse needs to be cached
  #
  # Returns:
  #   A matrix that is the inverse of 'x'
  
  # check if the inverse is already in the cache
  inverse <- x$getinv()
  
  if(!is.null(inverse)) {
     message("getting cached data")
     return(inverse)
  }
  
  # else get the matrix into matrixData
  matrixData <- x$get()
  
  # and compute the inverse of matrix fetched
  inverse <- solve(matrixData, ...)
  
  # then cache the inverse
  x$setinv(inverse)
  
  # and finally return the inverse
  inverse
}

## cacheMatrix.r
## Defines two functions to compute and cache the inverse of a matrix.
# 

## Function: makeCacheMatrix(x)
##  Creates a new matrix withe the value of x
##
## Exposes four functions from a list:
##  1. set(y):  Sets the value of the matrix and clears the inverse.  
##              This will cause the cached inverse to be recalculated in the cacheSolve function.
##  2. get():   Returns the value of the matrix
##  3. setInverse(inverse): Sets the inverse of the matrix
##  4. getInverse():  Returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() i
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function: cacheSolve(x)
##  Calculates the inverse of a matrix.  
##  If the value has previously been calculated and the matrix value has not changed, 
##  return the cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

## Usage and output
##
# > cm=makeCacheMatrix(matrix(rnorm(4),2,2))
# > cacheSolve(cm)
# [,1]       [,2]
# [1,]  0.45232761  0.7720369
# [2,] -0.01507577 -1.2847528
# > cacheSolve(cm)
# getting cached data
# [,1]       [,2]
# [1,]  0.45232761  0.7720369
# [2,] -0.01507577 -1.2847528
# > cm$set(matrix(rnorm(9),3,3))
# > cacheSolve(cm)
# [,1]       [,2]       [,3]
# [1,] -0.3666295  1.0454790   6.367042
# [2,]  0.2276199 -0.8585165 -10.649591
# [3,]  0.3536410  0.5995440   2.868472
# > cacheSolve(cm)
# getting cached data
# [,1]       [,2]       [,3]
# [1,] -0.3666295  1.0454790   6.367042
# [2,]  0.2276199 -0.8585165 -10.649591
# [3,]  0.3536410  0.5995440   2.868472

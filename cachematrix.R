## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix creates a special matrix object that can cache its inverse
##x needs to be a square matrix 
## returns a list containing functions to do the following
## 1. set the matrix 
## 2. get the matrix 
## 3. set the inverse 
## 4. get the inverse
# this is used an input to the cacheSolve() function


makeCacheMatrix <- function(x = matrix()) {
  ##variable to hold inverse
  inv <- NULL
  ##function to set values
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##get function 
  get <- function() x
  ##setinverse and get inverse to get and set the inverse of chached matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  ##list of functions to the cached object
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
}


## Write a short comment describing this function
##cacheSolve calculates the inverse of a matrix
##If the inverse has already been found and is cached, then cacheSolve retrieves it and if not computes the
##inverse matrix, caches it, and returns it.

cacheSolve <- function(x, ...) {
  ## get inverse of x and return it
          inv <- x$getInverse()
  ## is inverse already calculated? if so return value
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##calculate inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

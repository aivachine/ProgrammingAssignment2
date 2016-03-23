##  - Problematic -
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## makeCacheMatrix(matrix)
## This function creates a special "matrix" object that can cache its inverse.
## It represents a wrapper that provide getter and setter methods (matrix and matrix's inverse).

makeCacheMatrix <- function(x = matrix()) {
  
  # put the stored matrix's inverse value to NULL
  i <- NULL
  
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the value of a matrix's inverse
  setInverse <- function(inverse) i <<- inverse
  
  ## Get the value of a matrix's inverse
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve(matrix)
## This function computes the inverse of the special "matrix"  
## created by makeCacheMatrix above. If the inverse has already been calculated, 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  
  ## Check if the inverse was already stored
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
  ## Return of a matrix's inverse
    return(i)
  }
  
  data <- x$get()
  
  ## Inverse a matrix using the function - solve {base}
  i <- solve(data)
  
  ## Store in the cache
  x$setInverse(i)
 
   ## Return of a matrix's inverse
  i
}

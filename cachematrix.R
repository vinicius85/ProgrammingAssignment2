## makeCacheMatrix is intended to provide a list with several functions related to matrix caching, using R lexical scoping
## get() -> returns a matrix
## set() -> set a matrix to environment
## getInverse() -> returns an inverse matrix
## setInverse() -> set inverse matrix to environment

makeCacheMatrix <- function(matrix = matrix()) {
  
  #inverse matrix variable
  inverse <- NULL
  
  #store a matrix
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  
  #retrieve matrix
  get <- function() matrix
  
  #store inverse matrix
  setInverse <- function(x) inverse <<- x
  
  #retrieve inverse matrix
  getInverse <- function() inverse
  
  #return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


#cacheSolve attemps to get a cached inverse matrix version of original matrix.
#if inverse matrix is not stored in cache, then inverse matrix will be computed 
cacheSolve <- function(x, ...) {
  
  #attempt get inverse matrix
  inverse <- x$getInverse()
  
  #if inverse is not null, then return cached data
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #else get original matrix and compute its inverse
  data <- x$get()
  inverse <- solve(data)
  
  #store inverse
  x$setInverse(inverse)
  
  #return fresh inverse computation 
  inverse
}

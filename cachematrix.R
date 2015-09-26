## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(invP)  inverseMatrix<<- invP
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix = x$getInverse()
  if ( !is.null(inverseMatrix)){
    message("Getting cached data")
    return (inverseMatrix)
  }
  #no cached data calculate inverse
  myMatrix = x$get()
  inverseMatrix = solve(myMatrix,...)
  x$setInverse(inverseMatrix)
  return (inverseMatrix)
}
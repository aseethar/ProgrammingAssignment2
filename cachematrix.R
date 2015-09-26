## This R file has two functions makeCacheMatrix and cacheSolve

## makeCacheMatrix - this function creates the matrix and set the values in the matrix as well as the inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize inverse to NULL
  inverseMatrix <- NULL
 
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  #returns the matrix
  get <- function() x
  #sets the inverse of the matrix
  setInverse <- function(invP)  inverseMatrix<<- invP
  #gets the inverse of the matrix
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve - this function returns the inverse of the matrix if it is present the cache. 
## If there is no copy in the cache then it calculates the inverse using the solve function and
## stores the inverse in the cache

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

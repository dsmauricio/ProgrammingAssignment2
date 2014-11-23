## Function that holds Matrix caching functions inside
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the variable that holds de inverse matrix
  inverseMatrix <- NULL
  
  ## function that returns  the incomming matrix
  getOriginal <- function() x
  
  ## Function that returns the inverse matrix
  getInverse <- function() inverseMatrix
  
  ## function that solves the inverse of the matrix
  setInverse <- function(solved) inverseMatrix <<- solved
  
  ## List that returns the available functions.
  list(getOriginal=getOriginal, setInverse=setInverse, getInverse=getInverse)

}

## Function that returns cached matrix, or solve the matrix if it's not cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverseMatrix <- x$getInverse()
  
  if (!is.null(inverseMatrix)){
    ## Matrix is cached, use the cached matrix
    message("Using cached matrix")
    return(inverseMatrix)
  }
  
  ## Matrix is not cached solve it and cache it
  inverseMatrix <- solve(x$getOriginal(), ...)
  message("Caching Matrix")
  x$setInverse(inverseMatrix)
  return(inverseMatrix)
}

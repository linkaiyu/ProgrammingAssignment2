## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inverseX <- NULL                      # init inverseX to null
  
  set <- function(val) {
    x <<- val                           # cache matrix to x
    inverseX <<- NULL                   # set global inverseX to null
  }
  get <- function() x                   # return matrix
  
  setinverseX <- function(invx) inverseX <<- invx # cache inverse matrix
  
  getinverseX <- function() inverseX    # return cached inverse matrix
  
  list(set = set,                       # create a list that contains gets and sets
       get = get,                       #
       setinversX = setinversX,
       getinversX = getinversX)
  
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inverseX <- x$getinverseX()           # retrieve cached inverse matrix
  
  if(!is.null(inverseX)) {              # if found, return the cached inverse matrix
    message("getting cached data")
    return(inverseX)
  }
  
  data <- inverseX$get()                # otherwise retrieve the matrix
  
  tempInvX <- Solve(data, ...)          # compute the inverse
  
  x$setinverseX(tempInvx)               # save the computed inverse to cache

  tempInvx                              # return the computed inverse

}

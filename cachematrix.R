## These functions build a cache "matrix" - a vector of functions to get, set
## the given matrix, and with that have the ability to calculate the inverse
## a matrix, and cache the original matrices' inverse
## 

## This function creates a special "matrix" object that can cache its inverse.
## The matrix is a list of four named functions: 
## get - gets the original matrix
## set - sets the original matrix
## getInvMatrix - gets the inverse matrix
## setInvMatrix - gets the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y){
    x <<- y 
    invMatrix <<- NULL
  }
  get <- function() x 
  setInvMatrix <- function(im)  invMatrix <<- im
  getInvMatrix <- function() invMatrix
  
  #return special list, with functions
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
## Per assignment instructions: "assuming matrix is always invertible"
##
cacheSolve <- function(x, ...) {
  
  #getInvMatrix returns null if the matrix has changed: no need to double check
  invMatrix <- x$getInvMatrix()
  
  #check cache hit
  if(!is.null(invMatrix)) {
    message("getting cached matrix")
    return(invMatrix)
  }
  
  #otherwise, calculate the mean and cache it
  val <- x$get()
  invMatrix <- solve(val)
  x$setInvMatrix(val)
  
  invMatrix
}
## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
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
##
cacheSolve <- function(x, ...) {
  
  #does the inverse exist?
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
  
  val
  
  
}


#cachemean <- function(x, ...) {
#  m <- x$getmean()
#  if(!is.null(m)) {
#    message("getting cached data")
#    return(m)
#  }
#  data <- x$get()
#  m <- mean(data, ...)
#  x$setmean(m)
#  m
#}
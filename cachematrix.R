## Put comments here that give an overall description of what your
## functions do

## Create a matrix that can store a chached version of the inverse of itself.
## Call only using an invertable matrix, use 'variable$get()' to get the matrix, 
## and 'variable$getInverse()' to get its inverse after you have calculated it.
## Using cacheSolve

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(matrix){
       m <<- matrix
       inverse <<- null
  }
  get <- function() m
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, setInverse =setInverse, getInverse = getInverse)
}


## Check if already cached, if so return the inverse directly.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(is.null(inv)){
    x$setInverse(solve(x$get()))
    inv <- x$getInverse()
  }
  else{
    message("using cached data")
  }
  inv
}

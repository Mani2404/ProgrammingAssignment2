## The following functions would simplify the computation 
## of matrix inversion by caching the inverse of a matrix rather than calculating all the time,
## thereby saving a lot on unwanted runtimes

## Function to create a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inverse <<- inv
  getInv <- function() inverse
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If it has already been calculated and not null, then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInv()
  if(!is.null(inverse)){
    message("getting cached matrix")
    return(inverse)
  }
  a.data <- x$get()
  inverse <- solve(a.data,...)
  x$setInv(inverse)
  return(inverse)
}

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function generates a special matrix with a list of methods
## that get and set its data and cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(new_matrix)
  {
    x <<- new_matrix
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(new_inverse) inverse <<- new_inverse
  getInverse <- function() inverse

  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve function takes a CacheMatrix object (made with makeCacheMatrix)
## and returns its inverse.  If an inverse is previously cached, a cache is returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse))
  {
    message("using cached inverse")
    return (inverse)
  }
  inverse <- solve(x$get(),...)
  x$setInverse(inverse)

  inverse
}

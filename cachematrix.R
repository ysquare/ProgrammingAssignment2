## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

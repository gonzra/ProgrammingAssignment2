## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  matrixInverse <- NULL
  set <- function(y)
  {
    x <<- y
    matrixInverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) matrixInverse <<- inverse
  getInverse <- function() matrixInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse))
  {
    message("getting cached matrix inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

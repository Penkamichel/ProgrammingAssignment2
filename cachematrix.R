## Put comments here that give an overall description of what your
## functions do
# This function creates a speial "matrix" and compute the inverse of the "matrix"
# with caching the inverse of a matrix rather than compute it repeatedly.


## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
                  x <<- y
                  i <<- NULL
            }
      get <- function() x
      setInverse <- function(Inverse) i <<- Inverse
      getInverse <- function() i
      list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
      i <- x$getInverse()
      if(!is.null(i)){
            message("getting cached data")
            return(i)
      }
      matrix <- x$get()
      i <- solve(matrix, ...)
      
      ## Return a matrix that is the inverse of 'x'
      x$setInverse(i)
      i

}

# makeCacheMatrix is a function that creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {

  # holds the cached value if any, or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  cache <- NULL

  # set a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # as the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }

  # returns the stored matrix
  getMatrix <- function() {
    x
  }

  # cache the given argument
  cacheInverse <- function(solve) {
    cache <<- solve
  }

  # get the cached value
  getInverse <- function() {
    cache
  }

  # Return a list. Each named element of the list is function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a special matrix teturned above by
# makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return the value
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # get the matrix, calculate the inverse, and store it in
  # the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)

  # return the inverse
  inverse
}

#--------------------------------
# Yalena H. Chothia, 02.07.2020
# Programming Assignment 2: write a pair of functions that cache the inverse of a matrix.
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#--------------------------------

# 'makeCacheMatrix' is a function that makes a cache matrix from a given matrix
# The cache matrix is initialised by assigning the value 'NULL'
# 'setMatrix' assigns a value to x, giving a matrix
# 'getMatrix' returns the matrix assigned to x
# 'setCache' inverts the cached matrix
# 'getCache' returns the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix = NULL
  }
  getMatrix <- function() x
  setCache <- function(inverse) cacheMatrix <<- inverse
  getCache <- function() cacheMatrix
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
}

# 'cacheSolve' is a function that returns the inverse of a given matrix using the cache
# This is dependent on the contents of the cache.
# If the contents is not 'NULL' then the result is returned 
# If the contents is empty then the cache is created and set.
# The cache is updated and the cache matrix is returned
cacheSolve <- function(x, ...) {
  cacheMatrix <- x$getCache()
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
}
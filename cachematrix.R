## The following two functions can be used to cache and retrieve from cached memory the inverse of an
## invertible matrix.

## The first function (makeCacheMatrix) creates a "matrix" object that can cache the inverse of itself.
## Inside this function are a list of four other functions that aid in the caching process by setting 
## values, getting values, setting the inverse [of the matrix], and getting the inverse [of the matrix].

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function displays the inverse of a matrix if the inverse has already been calculated 
## and cached. If the inverse has not yet been calculated (i.e., the return value for the inverse is 
## NULL), the function computes and displays the inverse of the matrix using the cached values from 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
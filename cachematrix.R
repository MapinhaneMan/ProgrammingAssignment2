## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  in <- NULL
  set <- function(y) {
    x <<- y
    in <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) in <<- inverse
  getinverse <- function() in
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the inverse of the matrix
cacheSolve <- function(x, ...) {
  in <- x$getinverse()
  if (!is.null(in)) {
    message("getting cached data")
    return(in)
  }
  data <- x$get()
  in <- solve(data, ...)
  x$setinverse(in)
  in
}

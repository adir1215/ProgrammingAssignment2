## Put comments here that give an overall description of what your
## functions do: 
##    Just as the assignment asks, this pair of functions caches the 
##    inverse of a matrix through usage of the solve function and the code
##    detailed below. Assuming the matrix is invertible, by utilizing both 
##    these functions a person can easily obtain the inverse of any square 
##    matrix. 

## Write a short comment describing this function
##    This function makes a matrix that can have a cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
##    This function supplies the inverse of the matrix, whether it has
##    already been calculated or not.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  message("calculating inverse")
  m
}

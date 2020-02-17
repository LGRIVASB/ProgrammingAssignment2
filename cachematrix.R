## Put comments here that give an overall description of what your
## functions do
#Two functions that are used to create a special object that stores a matrix and cache's its inverse

## Write a short comment describing this function
#This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y #"<<-" operator which assing a value to an object in an environment that is different from the current environment.
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(solve) m <<- solve
  getMatrixInverse <- function() m
  list(set = set, get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function
#This function computes the inverse of the matrix returned by makeCacheMatrix. 
#If the inverse of the matrix has already been calculated, then the cacheSolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInverse()
  if(!is.null(m)){
    message ("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  m
}

## Programming Assignment 2: Taking advantage of Lexical Scoping rules in R

## A list containing functions to set and retrieve values of a matrix/calculate their inverse

makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Solve the matrix and check to see whether it's already stored in memory

  cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
  }

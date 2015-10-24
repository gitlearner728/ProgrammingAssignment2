## Caching Inverse of the matrix: Below two functions helps in increasing the performance by not running the
## inverse function for matrix with same values more than once.
##
##
## makecachematrix function takes matrix as input and creates a list of 4 functions.
## First one is the set function i.e. used to modify the matrix data and get function will return the matrix value.
## Similarly getinverse function returns the inverse value of the matrix and setinverse function updates
## inverse value to null if the set function is runs.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse value of x if it is null or else it returns the previously cached value

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


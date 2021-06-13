
## makeCacheMatrix creates a special matrix (like the special vector) which is a list containing a function to
## set the value of the matrix, get the value of the matrix then set the value of the inverse and get the value of the inverse
## i will be inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
## if it has not been calculated yet, then it will set the value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  OriginalMatrix <- x$get()
  i <- solve(OriginalMatrix, ...)
  x$setInverse(i)
  i
  
}

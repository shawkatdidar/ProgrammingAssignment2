## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment.
     x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function


## Return a matrix that is the inverse of 'x'
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  # if the inverse has already been calculated
  if (!is.null(i)) {
    # get it from the cache and skips the computation.
    message("getting cached data")
    return(i)
  }
  # otherwise, calculates the inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  # sets the value of the inverse in the cache via the setinv function.
  i
}
## A helper function for caching matrix inversion calculations
## and the solver invoking the wrapped matrix cache

## Create a wrapped matrix with caching
## x : matrix (numeric)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solve a matrix inverse using a caching wrapper
## x : makeCacheMatrix type
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## makeCacheMatrix accepts an invertable matrix as argument.set() resets values in case new matrix is passed to CachSolve
## All 4 functions are stored in a list

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function(){x}
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve retrieves inverse value from makeCacheMatrix environment if it exists for particular matrix
## If it is not solved, it calculates inverse and stores in i, ready to be called on later

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  d <- x$get()
  i <- solve(d, ...)
  x$setinv(i)
  i
}
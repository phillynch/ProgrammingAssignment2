## Put comments here that give an overall description of what your
## functions do

## This function takes a square matrix as an input.
## As per instruction there is no checking here it is assumed.
## 4 methods are provided.  set, get, setinv and getinv.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NUL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The cacheSolve function has a instance of the makeCacheMatrix function passed to it.
## It checks if m has a value other than NULL.
## If it is not NULL it returns the cached data in m
## If it is NULL it calcs the inverse assigns to m and returns m.


cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve (data, ...)
  x$setinv(m)
  m
  
        ## Return a matrix that is the inverse of 'x'
}

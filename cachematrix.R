## Below are two functions that are used to create a special object that stores 
## a matrix and caches its inverse

## this function makeCacheMatrix creates a vector, containing a function to set&get
## value of vector, and set&get inverse ma Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## calculates the inverse of a matrix created with above function

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m     ## Return a matrix that is the inverse of 'x'
}

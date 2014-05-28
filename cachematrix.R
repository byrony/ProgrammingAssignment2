## Below are two functions that are used to create a special object that stores 
## a matrix and caches its inverse

## this function makeCacheMatrix returns a list containing a function to 
## set the value of a matrix in cache
## get the value of a matrix from cache
## set the inverse of a matrix in cache
## get the inverse of a matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) I <<- solve
  getsolve <- function() I
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## get inverse of a matrix from cached data, or
## calculates the inverse of a matrix if it is not cached before
## the argument of cacheSolve() should be the return data of makeCacheMatrix()

cacheSolve <- function(x, ...) {
  I <- x$getsolve()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setsolve(I)
  I     ## Return a matrix that is the inverse of 'x'
}

## I completed the following two functions that cache the inverse of
## a matrix. The first function "makeCacheMatrix" caches the inverse 
## of a matrix. The second function "cacheSolve" computes the inverse
## of the matrix returned by the first function.
## The codes were adapted from the example "Caching the Mean of a
## Vector" provided with the assignment. Thank goodness for the example!


## makeCacheMatrix is a function that (hopefully) creates a "matrix"
## object that can cache its inverse:
##   set - set the matrix
##   get - get the matrix
##   setinmatrix - set the inverse of the matrix
##   getinmatrix - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinmatrix <- function(inmatrix) m <<- inmatrix
    getinmatrix <- function() m
    list(set = set, get = get,
         setinmatrix = setinmatrix,
         getinmatrix = getinmatrix)
}


## cacheSolve is a function that computes the inverse of the matrix
## returned by the makeCacheMatrix function. The if statement checks
## to see if the inverse matrix has already been calculated. If so,
## the matrix will be obtained from the cache instead of further
## computation.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinmatrix(m)
    m
}

## PS: Thank you for reviewing my work!

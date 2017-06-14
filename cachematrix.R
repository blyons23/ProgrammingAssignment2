## This program contains 2 functions - makeCacheMatrix() and cacheSolve(). The
## first function creates an R object that stores a matrix and its inverse. The
## second function cacheSolve() requires an argument that is returned by 
## makeCacheMatrix in order to retrieve the inverse from the cached value that is
## stored in the makeCacheMatrix object's environment.

## This function builds a set of functions and returns the functions within the
## list to the parent environment. The resulting object contains four functions: 
## set(), get(), setinverse(), getinverse(). It also contains 2 data objects: 
## x and m.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## Without cacheSolve, makeCacheMatrix is incomplete. As designed, cacheSolve()
## is required to populate or retrieve the inverse from an object of type 
## makeCacheMatrix()

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}

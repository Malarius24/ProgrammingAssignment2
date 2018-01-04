##The makeCacheMatrix function first creates a list of functions that sets or gets the matrix
##getinverse checks if the inverse already exists and returns the matrix if it does
##setinverse sets the inverse if it does not yet exists

##cacheSolve uses the list of makeCacheMatrix as input and returns the inverse of the matrix
##either from the cache or a new calculation

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

##example

##create a sample matrix

mtrx <- matrix(seq(1:9),3,3)

##create list for cache matrix 

cache <- makeCacheMatrix(cache)

##run with no matrix in cache

cacheSolve(cache)

##run with matrix in cache

cacheSolve(cache)
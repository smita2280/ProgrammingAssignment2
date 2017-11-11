

## This function creates a special "matrix" object that can cache its inverse, mainly a list containing 
##set the value of the matrix,get the value of the matrix
##set the value of the inverse,get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,get = get,
       setinverse = setinverse,getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache. Computing the inverse of a square matrix can be done with the solve function in R. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
  }
  matx <- x$get()
  inv <- solve(matx, ...)
  x$setinverse(inv)
  inv
}

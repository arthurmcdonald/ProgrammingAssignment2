# This function creates a 'special' matrix that
# will store a cached version in memory
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {
    x
  }
  set_inverse <- function(inv) {
    i <<- inv
  }
  get_inverse <- function() {
    i
  }
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


# This function will check if the matrix is in the cache. If so,
# it returns the cached version of the matrix, otherwise
# it will compute the inverse of the matrix (using R's 'solve()' function)
# store it in cache and then return the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data) # compute the inverse
  x$set_inverse(inv)
  inv
}




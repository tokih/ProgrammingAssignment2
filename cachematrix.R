# create a "cache matrix"
makeCacheMatrix <- function(x = matrix()) {
  # set the inverse to be NULL in the beginning
  inv <- NULL
  # a setter function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # a getter function
  get <- function() x
  # setter for the inverse, calulated by solve
  setInverse <- function(solve) inv <<- solve
  # getter for the inverse
  getInverse <- function() inv
  # storing all the functions into a list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# solve a "cache matrix"
cacheSolve <- function(x, ...) {
  # try and get the inverse stored by the "cache matrix"
  inv <- x$getInverse()
  # if a previously calculated inverse was found, return that along with a friendly message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # otherwise, get the data...
  data <- x$get()
  # ...and solve it..
  inv <- solve(data)
  # ..and lastly store the inverse into the object's cache...
  x$setInverse(inv)
  # ..and return the newly calculuated inverse
  inv
}
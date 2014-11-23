## The following functions are meant to create a list object containing
## some functions to be able to store a matrix and use it with it's
## inverse value without the need to recalculate it everytime.

## makeCacheMatrix function returns a simple list of
## 2 getter functions and 2 setters functions.
## in pratice this is just a data repository and it's name
## is actually not accurate because you can set any value to x and m

makeCacheMatrix <- function(x = matrix()) {
  inverse_value <- NULL
  set <- function(value) { 
    x <<- value
    inverse_value <<- NULL
  }
  get <- function() x
  set_inverse <- function(value) inverse_value <<- value
  get_inverse <- function() inverse_value

  list(set         = set, 
       get         = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve function returns a matrix that is the inverse of x
## It expects x being a matrix created by makeCacheMatrix function
## It calculates the inverse of x if it has not been calculated yet

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inverse_value <- x$get_inverse()
  if(!is.null(inverse_value)) {
    message("getting cached data")
    return(inverse_value)
  }

  inverse_value <- solve(x$get(), ...)
  x$set_inverse(inverse_value)
}

## testIt is meant to test the the other two functions behavior
testIt <- function() {
  a_matrix      <- rbind(c(1, 4), c(2, 3))
  inverse_value <- solve(a_matrix)
  true_matrix   <- inverse_value == inverse_value

  cache_matrix  <- makeCacheMatrix(a_matrix)

  if(!is.null(cache_matrix$get_inverse())) {
    return(message("New cache matrix should return null inverse value"))
  }

  if(cacheSolve(cache_matrix) != inverse_value && true_matrix) {
    return(message("cacheSolve function did not return the expected value"))
  }

  if(cache_matrix$get_inverse() != inverse_value && true_matrix) {
    return(message("cache_matrix$get_inverse() did not return the expected value"))
  }

  cache_matrix$set(a_matrix)
  if(!is.null(cache_matrix$get_inverse())) {
    return(message("cache matrix after a set call should return null inverse value"))
  }

  message("Everything is ok")
}


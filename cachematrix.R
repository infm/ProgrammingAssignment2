## Works pretty the same way as an example
makeCacheMatrix <- function(x = matrix()) {
  ## initial value of inversed must be NULL
  inversed <- NULL

  set_matrix <- function(m) {
    x <<- m
    ## invalidate inversed value
    inversed <<- NULL
  }
  get_matrix <- function() x

  set_inversed <- function(inv) inversed <<- inv
  get_inversed <- function() inversed
  
  ## returning list of auxilary functions
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inversed = set_inversed, get_inversed = get_inversed)
}

## Works pretty the same way as an example
cacheSolve <- function(x, ...) {
  ## trying to get cached value
  inversed <- x$get_inversed()
  if (!is.null(inversed)) {
    message("returning cached data")
    return (inversed)
  }

  ## otherwise - compute the inversed matrix
  data <- x$get_matrix()
  inversed <- solve(data, ...)

  ## caches value
  x$set_inversed(inversed)

  inversed
}

## Works pretty the same way as an example
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set_matrix <- function(m) {
    x <<- m
    inversed <<- NULL
  }
  get_matrix <- function() x
  set_inversed <- function(inv) inversed <<- inv
  get_inversed <- function() inversed
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inversed = set_inversed, get_inversed = get_inversed)
}

## Works pretty the same way as an example
cacheSolve <- function(x, ...) {
  inversed <- x$get_inversed()
  if (!is.null(inversed)) {
    message("returning cached data")
    return (inversed)
  }
  data <- x$get_matrix()
  ## The meat of the whole assignment
  inversed <- solve(data, ...)
  x$set_inversed(inversed)
  inversed
}

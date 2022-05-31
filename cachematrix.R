## The first function makeCacheMatrix creates a matrix which sets the values in the matrix, gets the values of the matrix, sets the values in the inversed matrix
## and gets the values of the inversed matrix
## For this I set the values in the matrix using a sample function
## I set the solved value 'inverse_matrix' as a null
## I changed any reference in the mean example from 'mean' to 'solve'


makeCacheMatrix <- function(x = matrix(sample(1:50,6),2,2)) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse_matrix <<- solve
  getsolve <- function() inverse_matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## The cacheSolve function calculates the inverse of the matrix, I created with set values 
## The function will first check to see if the inverse of this matrix has already been calculated. 
## If it has it will get the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix
## and will set the values of the inverse of the matrix in the cache via the setsolve function.
## Similarly as above I changed any reference in the mean example from 'mean' to 'solve'


cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getsolve()
  if(!is.null(inverse_matrix)) {
    message("getting inversed matrix")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setsolve(inverse_matrix)
  inverse_matrix
}

cacheSolve(makeCacheMatrix())



## This function creates a special "matrix" object that can cache its inverse.

##  Description of the funcation makeCacheMatrix 
##  set the value of the matrix
##  get the value of the matrix
##  set the value of inverse of the matrix
##  get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv_matrix <<- solve
  getinverse <- function() inv_matrix
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("Getting cached data!")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data)
  x$setinverse(inv_matrix)
  inv_matrix
}
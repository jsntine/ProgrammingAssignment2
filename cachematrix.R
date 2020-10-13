## 2 functions makeCacheMatrix and cacheSolve
## makeCacheMatrix consists of set, get, setinv, and getinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                ## initialize inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}      ## function to get matrix x
  setinv <- function(inverse) {inv <<- inverse} 
  getinv <- function() {inv} ## function to obtain the inverse of the matrix
  list(set = set, get =get, setinv = setinv, getinv = getinv)
}


## cacheSolve is used to get the cached data

cacheSolve <- function(x, ...){       ## get cached data
  inv <- x$getinv()
  if(!is.null(inv)){                  ## check if the inverse is NULL
    message("getting cached data")
    return(inv)                       ## returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)              ## calculate inverse value
  x$setinv(inv)
  inv                                 ## return the inverse of matrix 'x'
}
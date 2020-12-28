## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(newMatrix) {
    mat <<- newMatrix
    inv <<- NULL
  }
  get <- function() mat
  setInv <- function(newInv) inv <<- newInv
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
  inv <- mat$getInv()
  if(!is.null(inv)) {
    message("getting cached data...")
    return(inv)
  }
  matrix <- mat$get()
  inv <- solve(matrix, ...)
  mat$setInv(inv)
  inv
}
## makeCacheMatrix and cacheSolve work together to store an input nxn matrix 
## and calculates its inverse for future use. Matrices and inverses can be
## accessed with mat$get() and mat$getInv() defined functions.

## makeCacheMatrix takes an input nxn matrix and stores it in cache alongside 
## an initiated variables for its inverse.
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

## cacheSolve takes an input matrix and solves its inverse, if not already 
## solved, then stores the inverse in the supplied cache.
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
## This R.data file contains two functions. This first is makeCacheMatrix that stores a matrix
# into the cache (as a special variable). The cached matrix is stored as a list with 4 functions.
# These functions are used to get and set both the matrix and the inverse of the matrix
# The second function is cacheSolve. This function uses the solve() function to calculate the
# matrix inverse, it accepts a special/global or cached matrix defined by makeCacheMatrix as an
# argument. This function will first test whether the matrix inversion has already been calculated
# if it has, then inverted matrix will be called from the cache. If not, then it will calculate
# the matrix inversion and store the results into the cache, for future use.

## The makeCacheMatrix function:
# The makeCacheMatrix function creates a global matrix object/variable that cache's its inverse.
# The properties of the matrix are called using matrix$get, matrix$getinv. The matrix properties
# are set using matrix$set(y) and matrix.setinv()

makeCacheMatrix <- function(x = matrix()) {
  # Resets the variable m to NULL
  m <- NULL
  # Sets the matrix as a special/global matrix object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Retreived the original matrix
  get <- function() x
  # Stores the inverse matrix
  setinv <- function(solve) m <<- solve
  # Retrieves the inverse matrix 
  getinv <- function() m
  # Names the elements in the list and stores the functions in the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve function:
# This function computes the inverse of the global matrix returned by makeCacheMatrix above. 
# If the inverse has already been calculated, 
# then the cacheSolve function will retrieve the inverse of the matrix from the cache.

cacheSolve <- function(x, ...) {
  # Extracts the cached inverse matrix, if it does not exist NULL is returned
  m <- x$getinv()
  
  # Test if the inverse of the matrix has been calculated previously
  if(!is.null(m)) {
    message("Marix Inverse already calculated - Getting cached data")
    return(m)
  }
  
  # This code is run if there is no cached inverse of the matrix, this calculates the inverse
  # This code also stores the inverse matrix into the cache using x$setinv(m)
  matrixdata <- x$get()
  # Uses solve to invert the matrix
  m <- solve(matrixdata, ...)
  # Stores the inverted matrix into the cache
  x$setinv(m)
  # Displays (prints) the result of the matrix inversion
  m
  
}

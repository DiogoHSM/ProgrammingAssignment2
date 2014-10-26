## Functions to calculate and cache the inverse of matrix

## Create a matrix and cache of the inverse

makeCacheMatrix <- function(matrix.x = matrix()) {
  
  if (!is.matrix(matrix.x)) { #Check if it is a real matrix
    stop("Matrix test failed")
  }
  
  matrix.inv <- NULL
  
  set <- function(y) {
    matrix.x <<- y
    matrix.inv <<- NULL
  }
  
  get <- function() matrix.x
  setinverse <- function(solve) matrix.inv <<- solve #Invert matrix
  getinverse <- function() matrix.inv
  
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
  
}

## Function to check and/or create a inverted matrix cache

cacheSolve <- function(matrix.cache, ...) {
  matrix.inv <- matrix.cache$getinverse()

  if(!is.null(matrix.inv)) { #The chache exists
    return(matrix.inv)
  }
  matrix.Y <- matrix.cache$get() #Matrix to inverse
  matrix.inv <- solve(matrix.Y)
  matrix.cache$setinverse(matrix.inv)
  matrix.inv
  
}

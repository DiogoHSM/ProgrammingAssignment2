## Functions to calculate and cache the inverse of matrix

## Create a matrix and cache of the inverse

makeCacheMatrix <- function(matrix.x = matrix()) {
  
{ #Check if it is a real matrix
  if (!is.matrix(matrix.x)) 
    stop("Matrix test failed")
  }
  
#Initial inverted matrix
  matrix.inv <- NULL
  
  set <- function(y) {
    matrix.x <<- y
    matrix.inv <<- NULL
  }
  
  get <- function() matrix.x #Get the matrix of the function
  setinverse <- function(solve) matrix.inv <<- solve #Invert matrix
  getinverse <- function() matrix.inv #Get the inverted matrix
  
  #Return a list with properties to get/set the matrix or the inverted matrix
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
  
}

## Function to check and/or create a inverted matrix cache

cacheSolve <- function(matrix.cache, ...) {
  matrix.inv <- matrix.cache$getinverse() #Try to get the inverted matrix cache

  if(!is.null(matrix.inv)) { #The chache exists
    return(matrix.inv)
  }
  
  matrix.Y <- matrix.cache$get() #Matrix to inverse
  matrix.inv <- solve(matrix.Y) #Invert the matrix
  matrix.cache$setinverse(matrix.inv) #Set the inverted matrix cache
  matrix.inv #Return the inverted matrix
  
}

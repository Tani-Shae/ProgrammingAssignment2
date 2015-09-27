##This function creates a special "matrix" object that can cache its inverse
##---------Functions and thier purposes------------
##setMatrix - Assign/set the value of the matrix
##getMatrix - gets the value of the matrix
##matrixInverse - store the inverse of the matrix
##getInverse - returns the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {

  cacheVal <- NULL
  
  
  ## assign matrix val
  setMatrix <- function(matval) {
    x <<- matval
    
    cacheVal <<- NULL
  }
  
  ##Retrieve the matrix values
  getMatrix <- function() {
    x
  }
  
  ## cache the given argument 
  matrixInverse <- function(solve) {
    cacheVal <<- solve
  }
  
  ## get the cached value
  getInverse <- function() {
    cacheVal
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, matrixInverse = matrixInverse, getInverse = getInverse)
}


##The function below returns inverse of a matrix created using the makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
            ##retrieve the cache a val
            inverse <- x$getInverse()
            
            
            
            ## returns catch values
            
            if(!is.null(inverse)) {
              
              message("Loading cache values..")
              
              return(inverse)
            }
            
            data <- x$getMatrix()
            inverse <- solve(data, ...)
            x$matrixInverse(inverse)
            
            ##inverse is returned
            inverse
}

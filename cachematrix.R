## In this assignment I am creating two R functions. 
## makeCacheMatrix() and cacheSolve()



## This funtion takes a matrix as an argument and returns
## a special list does the following
## Sets the value of Matrix
## Gets the Value of Matrix
## Sets the value for Matrix Inverse
## Gets the value of Matrix Inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <-NULL
  setMatrix <- function(){
    
    x <<- y
    
    inv <<- NULL
    
  }
  
  getMatrix <- function() x
  
  setInverse <- function(solve) inv <<- solve
  
  getInverse <- function() inv
  
  list(setMatrix = setMatrix , getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  
  
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then this method calculates the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    
    message("Getting Inverse of Matrix")
    
    return(inv)
  }
  
  thismatrix <- x$getMatrix()
  
  inv <- solve(thismatrix)
  
  x$setInverse(inv)
  
  inv
  
}
install.packages("tidyverse")
library(tidyverse)


##creates matrix that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL

##setting the matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }

##getting the matrix
  get <- function() {
    #return matrix
    m
  }

##setting inverse of matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }

##getting inverse of matrix
  getInverse <- function() {
    i
  }
  
##return list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##return matrix that is inverse of "x"
cacheSolve <- function(x, ...) {
  m <- x$getInverse()

##return only inverse if already set
  if ( !is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  
##get matrix from object
  data <- x$get()
  m <- x$get()

##calucalting the inverse with matrix multiplication
  m <- solve(data) %*% data
  
##setting inverse to object
  x$setInverse(m)
  
##returning matrix
  m
}




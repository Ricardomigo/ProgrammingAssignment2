## These are a pair of functions that cache the inverse matrix of a matrix.
## So if the results are cached, the computer can skip the step where it tries to calculate the inverse matrix.
## This would save a costly computation step.

## makeCacheMatrix is a function that can cache the inverse of a matrix. I does the following:
#initialize the inversematrix property
#'Set' is a way to set the matrix that is going to be used and cached
#'Get' helps get the matrix that is going to be used
#'setInverse' helps set the calculated inverse of the matrix
#'getInverse' helps get the calculated inversed matrix
#This lists the value of the four function in makeCacheMatrix


makeCacheMatrix <- function( cmatrix = matrix() ) {
      inversematrix <- NULL
      set <- function( y ) {
            cmatrix <<- y
            inversematrix <<- NULL
      }
      get <- function() {
            cmatrix
      }
      setInverse <- function(solve) {
            inversematrix <<- solve
      }
      getInverse <- function() {
            inversematrix
      }
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This fucntion works with makeCacheMatrix to check if the inverse matrix of a matrix has already been calculated.
##If it is calculated it returns the cached value.
## If it's not it identifies the matrix we are working with and it calculated the inverse matrix.
##It does the following:

## Return a matrix that is the inverse of 'acmatrix'
#This checks if the inverse has been calculated. If so it returns a message and then it's value.
#If the inverse isn't calculated, the original matrix is identified
#the inverse is calculated for the original matrix
#the inverse is then assigned to setmean in the makeCacheMatrix function
#the inverse is returned

cacheSolve <- function(acmatrix, ...) {
      inverseCheck <- acmatrix$getInverse()
      if( !is.null(inverseCheck) ) {
            message("getting cached data")
            return(inverseCheck)
      }
      data <- acmatrix$get()
      inverseCheck <- solve(data) %*% data
      acmatrix$setInverse(inverseCheck)
      inverseCheck
}

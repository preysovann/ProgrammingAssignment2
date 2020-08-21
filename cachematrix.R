## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMtx <- NULL
  setMatrix <- function(y){
    x <<- y
    invMtx <<- NULL
  }
  getMatrix <- function() x
  setInvMatrix <- function(iMtx) invMtx <<- iMtx
  getInvMatrix <- function() invMtx
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMtx <- x$getInvMatrix()
  if(!is.null(invMtx)){
    message('Getting cached data.')
    return(invMtx)
  }
  mtx <- x$getMatrix()
  invMtx <- solve(mtx,...)
  x$setInvMatrix(invMtx)
  invMtx
  
}

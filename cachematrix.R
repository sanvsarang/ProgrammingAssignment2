## makeCacheMatrix function creates a special matrix which can cache its inverse
## cacheSolve will try to retrieve cached inverse of the matrix first, else it will compute the matrix inverse
## For example, these commands can be run
## spl_m <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## the above will create special matrix
## cacheSolve(spl_m)
## on first run cacheSolve will compute inverse
## on second run cacheSolve will return cached inverse 

## makeCacheMatrix function creates a special matrix which can cache its inverse

makeCacheMatrix <- function (x = matrix()) {
  invx <<- NULL
  list( 
    set = function(y_param) {
      x <<- y_param 
      invx <<- NULL
    },
    get = function () {x},
    setInverse = function (invx_param) {invx <<- invx_param},
    getInverse = function () {invx}
  )
}

## cacheSolve will try to retrieve cached inverse of the matrix first, else it will compute the matrix inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getInverse()
  if (!is.null(invx)) {
    message("getting cached data")
    return(invx)
    }
  else {
    invx <- solve(x$get())
    x$setInverse(invx)
    invx
  }
  
}
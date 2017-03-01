## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix(x) takes a square matrix and calcuates the inverse of the matrix by using
## the solve(x) function.
##
## set() - stores the matrix of which the inverse is to be found, returns NULL
## get() - obtains the matrix that is being used, returns a matrix
## setinverse() - evaluates the inverse of the matrix provided and stores
## the inverse for later user, returns a matrix
## getinverse() - if the inverse has already been calculated, returns the stored inverse matrix, 
## returns a matrix
##
## the result of the function is a list that contains four functions

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function()
            x
      setinverse <- function(solve)
            m <<- solve
      getinverse <- function()
            m
      list(
            set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse
      )
}


## Write a short comment describing this function
##
## cachesolve uses the output of the makeCacheMatrix as its argument
##
## the first command, m <- x$getinverse(), tests to find if the inverse has already
## been calculated for this matrix.
## If FALSE, then the inverse is calculated using the solve() function
## If TRUE, then the inverse returned is the previous stored value that is in the list v
##
## Returns the inverse of the matrix that was originally used as an argument to makeCacheMatrix()
##
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

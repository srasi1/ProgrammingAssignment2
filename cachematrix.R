## The functions here are going to caculate the inverse of a matrix
## if the inverse is already calcuated for a specific matrix variable
## and is in memory, it is not recomputed again but is pulled from memory

## makeCacheMatrix function is to set and get the data input and output

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      get <- function() x
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      getinv <- function() i
      setinv <- function(inv) i <<- inv
      list(set = set, get = get, getinv = getinv, setinv = setinv)

}


## cacheSolve calculates the inverse of the matrix using Solve
## function. If inverse is already calcuated, it simply calls it 
## from cached memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}

## Functions to practice the concepts of lexical scoping in R
## In this example the functions store the value of the inverse of a matrix so as to avoid repeating the calculation
## later in the program.

## Define the structure of the object that stores and retrieves the inverse matrix using the functions:
##
##          set: initializes the value of the inverse matrix to NULL and stores the matrix
##          get: retrieves the matrix
##          setinv: stores the inverse
##          getinv: retrieves the inverse
makeCacheMatrix <- function(x = matrix()) {

            inv <- NULL
      
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      
      setinv <- function(inverse) inv <<- inverse
      
      getinv <- function() inv
      
      ## Returns the list of functions to be used later in cacheSolve
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Computes the inverse of the matrix using the functions defined in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      ## get the stored inverse matrix
      inv <- x$getinv()
      
      ##if is not NULL means that the inverse has been calculated previouly and returns it 
      if(!is.null(inv)) {
            message("getting the inverse matrix from the cache")
            return(inv)
      }
      
      ## if the program arrives here means that the inverse must be calculated
      
      ## get the matrix to be inverted
      data <- x$get()
      
      ## compute the inverse
      inv <- solve(data, ...)
      
      ## store the inverse
      x$setinv(inv)
      
      ## return the inverse
      inv
}

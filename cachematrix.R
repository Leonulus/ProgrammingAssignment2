## These functions do matrix inversion 


## This function creates an object which is matrix with methods to cache data and to extract
## computed data

makeCacheMatrix <- function(x = matrix()) {       # takes a matrix

      m <- NULL                                   # nullifies the inversion each time before solving
      set <- function(y) {                        # calls cacheSolve
            x <<- y
            m <<- NULL
      }
      get <- function() x                         # returns original matrix
      setinvert <- function(solve) m <<- solve    # called by cacheSolve for the first time
      getinvert <- function() m                   # returns cached value 
      list(set = set, get = get,                  # list of methods
           setinvert = setinvert,
           getinvert = getinvert)
}


## This function atually computes a data

cacheSolve <- function(x, ...) {                  # takes an object created by makeCahceMatrix
      
      m <- x$getinvert()                          # gets cached value
      if(!is.null(m)) {                           # if data is already  cached
            message("getting cached data")
            return(m)
      }
      data <- x$get()                             # if null, gets original matrix
      m <- solve(data, ...)                       # solving for the first time
      x$setinvert(m)                              # storing computed value
      m
        ## Return a matrix that is the inverse of 'x'
}

## These functions create a matrix object, allowing you to store
## both the values of the matrix and the values of its inverse. 
## Storing the inverse with the matrix can save time if you 
## need to use the inverse multiple times. However, you can
## choose to not calculate the inverse if you find 
## it unneccessary. 

## This function creates a matrix object with a list of four 
## methods: set(), get(), setinverse(), and get(inverse)
##  
##  set: takes a matrix and stores the values in the object 
##  get: takes no arguments and returns the matrix itself
##  setinverse: calculates the inverse of the matrix and stores
##              it in the object
##  getinverse: returns the invere of the matrix, if it exists

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a matrix object returned from makeCacheMatrix
## and returns its inverse. If the inverse has not yet been
## stored, it is calculatd using the setinverse method from 
## makeCacheMatrix. If it does it exist, the previously stored
## inverse is returned along with the message "getting cahced data". 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  
}

#This file does two main tasks: it solves for the the
#inverse of a specified matrix, and it saves (or caches)
#the result for retrieval at a later point. The purpose
#of caching it is to avoid repeating lengthy calculations.


#This function creates a four member list of functions (set,
#get, setInv, and getInv). The function employs the <<- operator
#to avoid allowing the objects within the function from being
#recognized outside in the global environment.

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inv) xinv <<- inv
  getInv <- function() xinv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


#This function solves for and returns the inversion of
#the matrix.

cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}

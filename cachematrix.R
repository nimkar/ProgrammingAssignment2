## cachematrix.R has two functions. One is a generator function that generates a matrix .  
## Second function is a modified version of solve that that works on the cached matrix object 

## create a matrix object that is capable of storing it's inverse 
## provide getter and setter methods for matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)
}


## Function to compute the inverse of matrix on the cachematrix object
## function tries to fetch inverse from cache first if it exists.
## if not it computes the inverse and stores it in the cache for future retrival

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Found cached matrix")
    return(inv)
  } else {
    message("Matrix not found in cache. Computing and adding to cache")
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    return(inv)
  }
}

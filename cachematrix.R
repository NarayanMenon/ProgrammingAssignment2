## This program contains two functions. The first will allow you to
## create a special matrix that allows caching of its inverse. The second
## is a function that will return the inverse from cache or compute it for the 
## first time

## Function to create a special matrix that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getInv <- function() inv
    setInv <- function (y) inv <<- y
    list(get=get,set=set,getInv=getInv,setInv=setInv)
}


## Function that will return the inverse from cache or will determine inverse
## and populate cache for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInv()
      if (!is.null(inv)) {
          message("using cached inverse")
          return(inv)
      }
      x_mat <- x$get()
      x_inv <- solve(x_mat,...)
      x$setInv(x_inv)
      x_inv
}

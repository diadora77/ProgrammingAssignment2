#   Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than compute it repeatedly (there are also alternatives 
# to matrix inversion that we will not discuss here). Note that this code assumes
# that the matrix is invertible.  
  
#   makeCacheMatrix: This function creates a special "matrix" object that can cache its 
# inverse.  Aspects of the object are:
#     $set, $get, $setinv, and $getinv

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#   cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
# not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  #checks to see if cache data exists, if not, computes, caches, and returns inv.
  #if it does exist, simply returns the cache value
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  
  ## Return a matrix that is the inverse of 'x'
  xinv
}

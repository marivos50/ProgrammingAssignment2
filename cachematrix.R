# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly (there are also alternatives to
# matrix inversion that we will not discuss here). 
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
                x <<- y
                m <<- NULL
                }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  m <- x$getsolve()
  
  if(!is.null(m)) {              #we already have computed the inverse
    message("getting cached data")
    return(m)                    #returning the inverse from the cache
  }
  data <- x$get()
  d<-dim(data)[1]
  I<-matrix(rep(1,d*d),nrow=d)   #creating an identity matrix of same size
  m <- solve(data, I)            #computing the inverse
  
  x$setsolve(m)                  #saving the inverse so we can use it later on
  m                              #returning the inverse calculated
}


#These two functions will cache the inverse of a matrix
#makeCacheMatrix makes a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL #clears the inverse to be prepared for the new calculation
  set <- function(y) { #sets values
    x <<- matrix(y,nrow=2,ncol=2)
    m <<- NULL
  }
  get <- function() x
  setInverse <-  function(){ 
    m <<- solve(x)    #this calculates inverse
  }
  
  getInverse <- function() m #displays inverse
  
  list(set = set, get = get,  #creates list of functions
       setInverse = setInverse,
       getInverse = getInverse)
}

#cacheSolve computes the inverse of the matrix. If the inverse has already
#been calculated then the cacheSolve will retreive the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
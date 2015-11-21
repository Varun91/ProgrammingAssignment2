

#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

    #set the value of the vector
    #get the value of the vector
    #set the value of the mean
    #get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Calculates the Inverse 
cacheInverse <- function(x, ...) {
  ## Return a matrix that is the inverse of x
  m <- x$getinverse() # gets the inverse 
  if(!is.null(m)) {   # if the inverse is not null, returns the cached value
    message("getting cached data")
    return(m)
  }
  data <- x$get()  # gets the value of x
  m <- solve(data, ...) # calculates the inverse
  x$setinverse(m) # sets the value of inverse in the object
  m               # returns the value of calculated inverse
}

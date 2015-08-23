
## The function makeCacheMatrix will initialize and cache the matrix object into its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # initialize to NULL
  cache <- NULL 
  # set the matrix in the working environment
  set <- function(y){
    # assign using operator for object in environment different from current environment
    x <<- y
    cache <<- NULL
  }
  
  # create matrix in current working environment
  get <- function() x
  
  # invert matrix and store results in cache
  setInverted <- function(inverse) cache <<- inverse
  
  #get inverted matrix from cache
  getInverted <- function() cache
  
  
  # return the functions results to the working environment
  list(set = set, get = get,
       setInverted = setInverted,
       getInverted = getInverted)
  
}


## The cacheSolve function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  #Get value of cache in 'int'
  inv <- x$getInverted()
  
  #if cache is not empty, display message that value retrieved from cache
  if (!is.null(inv)) {
    message("getting cached data")
    
    #display in console
    return(inv)
  }
  
  #Create matrix to assign result 
  mat <- x$get()
  
  #set return inverse of matrix
  inv <- solve(mat, ...)
  
  #set inverted matrix in cache
  x$setInverted(inv)
  
  # return value 
  inv
}

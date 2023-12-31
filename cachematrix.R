# Cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  get <- function() {
    m
  }

    setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ##Inverse of the matrix
  getInverse <- function() {
      i
  }
  
  ## Return 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInverse(m)
  
  m
}
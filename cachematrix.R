## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## set the inverse
  m <- NULL
  
  ## set matrix
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
  
  ## get matrix
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  ## generate a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## calculate inverse if missing
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  
  ## print inverse matrix
  m
}

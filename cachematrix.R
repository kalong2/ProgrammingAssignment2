## These functions take in a matrix, checks to see if the inverse is cached, and if it is not, solves the inverse and caches it


## makeCacheMatrix creates a special "vector", which is really a list containing functions that set/get the vector containing the functions
## or set/get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(solve) {
    inverseMatrix <<- solve
  }
  
  getInverse <- function() {
    inverseMatrix
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}





## cacheSolve with solve the inverse of the matrix, but will first check to see if the inverse is already cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  
  if(!is.null(inverseMatrix)) {
    message("Getting cached matrix inverse")
    return(inverseMatrix)
  }
  
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setInverse(inverseMatrix)
  inverseMatrix
}




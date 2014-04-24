## makeCacheMatrix and cacheSolve collectively allow the caching of computations, to prevent computations being executed each time it is called

## makeCacheMatrix creates and returns a list of 4 functions, which do the following
##1. Set the matix - set
##2. Get the matrix - get
##3. Set the inverse of the matrix - setinverse
##4. Get the inverse of the matrix - getinverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve takes the list of 4 functions created by makeCacheMatrix, and does the following
##1. Extracts the getinverse
##2. If it is not null (there is cache), it returns the value from cache
##3. If it is null, then it extracts get (the matrix), inverses it using solve(), sets the result to setinverse in cache, and returns the inverse

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
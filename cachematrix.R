## The task was to create two functions that cache the inverse of a matrix.


## The first function creates the matrix, and the special object that 
## will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inver <- NULL
      set <- function(x){
          x <<- y
          inver <<- NULL
      }
      get <-- function() {x}
      setInverse <- function(inverse) {inver <<- inverse}
      getInverse <- function() {inver}
      list( set = set, get = get, 
            setInverse = setInverse, 
            getInverse = getInverse)
}


## The second function computes the inverse of the special "matrix" object.
## The inverse has already been calculated and the matrix has not changed, so
## it will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
      inver <- x$getInverse()
      if(!is.null(inver)){
            message("getting cached data")
            return(inver)
      }
      mat <- x$get()
      inver <- solve(mat, ...)
      x$setInverse(inver)
      inver
}


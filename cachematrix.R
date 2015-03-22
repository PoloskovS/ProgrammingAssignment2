##This is a pair of functions that cache and compute the inverse of a matrix.

##Function makeCacheMatrix creates a special 
##"matrix" object that caches its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
	s <- NULL
	set <- function(y) {
		mtx <<- y
		s <<- NULL
      }
	get <- function() mtx
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

##Function cacheSolve computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(mtx, ...) {
	s <- mtx$getsolve()
      if(!is.null(s)) {
              message("Getting cached data...")
              return(s)
      }
      data <- mtx$get()
      s <- solve(data, ...)
      mtx$setsolve(s)
      s     
}

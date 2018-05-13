#
#These two functions: makeCacheMatrix  and cacheSolve combine to create and cache the
#inverse of an object (a matrix object).
#

      makeCacheMatrix <- function(x = matrix()) {

#
#Creates, caches and supports a list of functions that are used to return the
#inverse of a matrix object if one exists(cached) or, creates a new one instead.
#
        
          matrixInverse <- NULL
          set <- function(y) {
          x <<- y
          matrixInverse <<- NULL
         }
#
          
        get <- function() x
        setmatrixInverse <- function(matrixInverse) matrixInverse <<- matrixInverse
         getmatrixInverse <- function() matrixInverse
            list(set = set, get = get, setmatrixInverse = setmatrixInverse, 
            getmatrixInverse = getmatrixInverse)
      }
      
# 
#This function computes the matrixInverse of the object created by the makeCacheMatrix()
#function. But it returns the cached value if one exists and has not changed.
#
      
         cacheSolve <- function(x, ...) {
        matrixInverse <- x$getmatrixInverse()
         if (!is.null(matrixInverse)) {
            message("getting cached data.")
            return(matrixInverse)
         } 
        else {
          data <- x$get()
          matrixInverse <- solve(data)
          x$setmatrixInverse(matrixInverse)
          matrixInverse
       }
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#'First we define the function to set the value of the matrix which also clears
#'the old inverse value and then we proceed. We set the value and clear the cache
#'then we define the function for setting the inverse. This is only used by getinverse()
#'when there is no cached inverse

makeCacheMatrix <- function(x = matrix()) {
	mz <- NULL
    set <- function(y) {
        x <<- y    
        mz <<- NULL 
    }
       get <- function() x
        setInverse <- function(inverse) mz <<- inverse
        getInverse <- function() mz
        list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

#' Return inverse of matrix x
#' 
#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the cachesolve retrieves the 
#' inverse from the cache.


cacheSolve <- function(x) {
       mz <- x$getInverse() 
    if(!is.null(mz)) { # If the cache was not empty, we can just return it
        message("getting cached data")
        return(mz)
    }
    data <- x$get() 
    mz <- solve(data) 
    x$setInverse(mz)  
    mz                
}

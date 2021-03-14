## Put comments here that give an overall description of what your
## functions do

## The first function, `makeCacheMatrix` creates a special "matrix".

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
            set <- function(y) {
                    x <<- y
                    invmat <<- NULL
}
            get <- function() x
            setInverse <- function(inverse) invmat <<- setInverse
            getInverse <- function() (invmat)
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}


## Computing the inverse of a square matrix can be done with the `solve`function in R.

cacheSolve <- function(x, ...) {
      
            invMat <- x$getInverse()
            if(!is.null(invMat)) {
                    message("getting cached data")
                    return(invMat)
            }
            data <- x$get()
            invMat <- solve(data, ...)
            x$setInverse(invMat)
            invMat
    

}

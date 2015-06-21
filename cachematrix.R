## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i  <- NULL
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
        get  <- function() x
        setinvr  <- function(inverse) i  <<- inverse
        getinvr  <- function() i
        list(set= set, get = get, 
             setinvr = setinvr, 
             getinvr = getinvr)
        
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        i  <- x$getinvr()
        if (!is.null(i)){
                message("Fetching cached data")
                return(i)
        }
        data  <- x$get()
        i  <- solve(data, ...)
        x$setinvr(i)
        i
}

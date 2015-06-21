## At times, there is a benefit to caching inverse of a given
## matrix rather than compute it repeatedly. The following two functions
## cache the inverse of a matrix. At the end of this file is
## steps how to test this functions.

## MakeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse.

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


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

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

###############################
#### To test the function - follow this steps
###############################
####
#### Step 1: Create a two by two metrix - "m <- matrix(c(1, 2, 3, 4), 2,2)"
#### Step 2: Find the inverse of the metrix using the solve function  and save this 
#### to compare aginst the result "solve(m)"
#### Step 3: Now create the matrix ans sore in in var x  "x <- makeCacheMatrix(m)"
#### Step 4: View the stored matrix with the following command "x$get"
#### Step 5: Store the inverse of the cahse in a variable "inver <- cacheSolve(x)"
#### Step 6: Call the cached inverse with "inver"
####
####
################################
################################


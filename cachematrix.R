## Reducing computing costs by caching the inverse of a matrix
## so that it does not have to be computed again if it didn't change

## Creates an object that is capable of storing the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL       ## sets the inverse of the matrix initially to NULL, since it is not computed yet
    set <- function(y) {    ## creates a function with which to reset the object
        x <<- y             ## sets the new matrix y to be used for the to be inversed matrix x
        invMatrix <<- NULL  ## sets the inverse of the matrix back to NULL since the new inverse is not computed yet
    }
    get <- function() x     ## returns the matrix x, for which the inverse matrix can be stored
    setinverse <- function(inv) invMatrix <<- inv  ## stores the computed inverse for the matrix in invMatrix
    getinverse <- function() invMatrix             ## returns the stored inversed matrix
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)  ## returns the object with all four functions
}


## checks if the special "matrix"-object, which can store the inverse of the matrix,
## already contains the inverse of the matrix. If so, it gives back the inverse matrix from cache. 
## If not it computes the inverse matrix and stores it in the cache

cacheSolve <- function(x, ...) { ## is called with a special matrix-object, not the matrix directly
    invMatrix <- x$getinverse()  ## retrieves the value stored for the inverse matrix in the special "matrix"-object
    if (!is.null(invMatrix)) {   ## checks if the value of the inverse matrix is not NULL, that means there is
                                 ## a computed and stored inverse matrix already present
        message("getting cached data")  ## it informs the user that it will get the cached data, not compute anew
        return(invMatrix)        ## return the stored inverse matrix, without calculating it again
    }
    data <- x$get()              ## collects the stored matrix from the special matrix object
    invMatrix <- solve(data,...) ## computes the inverse of the matrix
    x$setinverse(invMatrix)      ## store the newly computed inverse of the matrix in the special matrix-object 
                                 ## for further use
    invMatrix                    ## returns the calculated inverse of the matrix
}

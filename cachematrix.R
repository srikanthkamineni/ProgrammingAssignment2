## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## inv will store the cached inverse matrix
        inv <- NULL
        
        ## set() clears both the data and the cached inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get() retreives the matrix encapsulated by the object
        get <- function() x
        
        ## setinv() saves the inverse to the cache
        setinv <- function(inverse) inv <<- inverse
        
        ## getinv() retrieves the cached inverse
        getinv <- function() inv
        
        ## Return the matrix 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## return the inverse from the cache if it has been cached already
        inv <- x$getinv()                
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        ## Cache the inverse
        x$setinv(inv)
        
        ## Return it
        inv
}

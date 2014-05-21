## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly.The following two functions are used to cache the 
## inverse of a matrix.

## makeCacheMatrix: return a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse
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
        
## cacheSolve: Computes the inverse of the matrix. If the inverse already
## exists before the calculations, it returns the cached inverse.
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

## Sample run:
## > x = rbind(c(1, -1/6), c(-1/6, 1))
## > cx = makeCacheMatrix(x)
## > cx$get()
## [,1]       [,2]
## [1,]  1.0000000 -0.1666667
## [2,] -0.1666667  1.0000000

## No cache during the first time run
## > cacheSolve(cx)
## [,1]      [,2]
## [1,] 1.0285714 0.1714286
## [2,] 0.1714286 1.0285714

## Retrieving from the cache in the second time run
## > cacheSolve(cx)
## getting cached data
## [,1]      [,2]
## [1,] 1.0285714 0.1714286
## [2,] 0.1714286 1.0285714
## >

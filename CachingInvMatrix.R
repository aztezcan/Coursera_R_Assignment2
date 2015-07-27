## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(matrix1 = matrix()) {
        inv <- NULL
        set <- function(x) {
                matrix1 <<- y
                inv <<- NULL
        }
        get <- function() return(matrix1)
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() return(inv)
        return(list(set = set, get = get, 
                    setinverse = setinverse, 
                    getinverse = getinverse))
}

## cacheSolve: This function computes 
## the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(matrix1, ...) {
        inv <- matrix1$getinverse()
        if(!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        data <- matrix1$get()
        inv <- solve(data, ...)
        matrix1$setinverse(inv)
        return(inv)
}
done
## makeCacheMatrix and cacheMatrix. Computing the inverse of a matrix
## is a costly operation. The following functions can be used to cache
## the result of calculating a matrix inverse and retrive the cached value
## when it's needed.

## makeCacheMatrix takes a square matrix as a parameter and returns
## a list of functions that set the marix value, get the matrix value,
## set the inverse value, and get the inverse value.

makeCacheMatrix <- function(x = matrix()) {
	## Clear out the inverse
        inv <- NULL ## inv will store the cached inverse of matrix x
        set <- function(y) {
                x <<- y
                inv <<- NULL	## matrix changed, clear inverse cache
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	## returns list of functions
}


## cacheSolve takes a list vector that has been created by makeCacheMatrix.
## If the cache is empty the inverse of the matrix is calculated,
## displayed, and cached. If the inverse is cached already, it is simply
## returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {	## check if inverse exists in the cache
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)	## solve() will calculate the inverse
        x$setinverse(inv) ## store and cache the inverse
        inv
}


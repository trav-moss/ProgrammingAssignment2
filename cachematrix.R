## Creates a special matrix object that can cache its inverse
## and then computes the inverse of the special matrix; if the
## inverse has already been calculated (and the matrix has not
## changed), then the value is retrieved from the cache.

## Make a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y = matrix()){
                x <<-y
                inv_m <<- NULL
        }
        get <- function(){x}
        setInv <- function(inv){
                inv_m <<- inv
        }
        getInv <- function(){inv_m}
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Compute the inverse of the matrix or retrieve a cached value
cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getInv()
        if(!is.null(inv_m)){
                message("getting cached data")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setInv(inv_m)
        inv_m
}

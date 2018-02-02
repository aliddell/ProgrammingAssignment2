## makeCacheMatrix: construct a dummy "CacheMatrix" class containing x and x^-1
## cacheSolve: query CacheMatrix for its inverse, compute it if not found
## invTest: sanity check on CacheMatrix and its inverse

## Constructs a thin wrapper around x containing getters/setters
#  for x and x^-1

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(p) {
                x <<- p
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Attempts to retrieve the inverse of x, if already cached
#  If not cached, computes the inverse and caches it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("cache hit")
                return(inv)
        }
        
        inv <- solve(x$get(), ...)
        x$setinv(inv)
        
        inv
}

## Returns the matrix norm of x * x^-1 (should be 1)
invTest <- function(x, ...) {
        y <- cacheSolve(x, ...)
        
        norm(x$get() %*% y)
}
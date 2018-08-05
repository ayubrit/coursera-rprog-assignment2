makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse)inv <<- inverse
        getInverse <- function()Inv
        list(set = set, 
		 get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}


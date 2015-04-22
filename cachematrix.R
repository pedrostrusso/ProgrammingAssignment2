makeCacheMatrix <- function(x){
        i <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...){
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i              
}
##The functions take a matrix and calculate the inverse of the matrix. The functions also cache
##the returned inverse matrix such that if the inverse has been calculated once, no time gets wasted
##in computing the inverse again. Instead, it simply gets returned from the cache.

##The makeCacheMatrix function takes a matrix as input and sets the value of the matrix, gets
##the value of the matrix, sets the value of the inverse and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##The cacheSolve function checks if the inverse of the special matrix created with the function
##above has been calculated previously. If yes, the function returns the inverse of the special 
##matrix from the cache. If not, the inverse gets calculated, returned and saved in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
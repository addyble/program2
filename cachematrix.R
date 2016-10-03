##1st funtion: Matrix setup
##2nd function: Inverse matrix calculation

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y) {
      x <<- y
      m <<- NULL
    }
    get <-function() x
    setmatrix <- function(solve) m<<- solve
    getmatrix <- function() m
    list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        cache <- x$getInverse()
        if (!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        matrix <- x$get()
        tryCatch( {
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)

                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)

                return(NA)
        },
        finally = {
                x$setMatrix(cache)
        } )
        return (cache)
}
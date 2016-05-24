#' 
#' This function creates a special "matrix" object that can cache its inverse
#' 
#' @param x the matrix to be cached
#' 
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    # sets matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # gets matrix
    get <- function()
        x
    # inverses matrix
    setInverse <- function(inverse)
        inv <<- inverse
    
    # returns inverse matrix
    getInverse <- function()
        inv
    
    # creates a list of existing methods
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

#'  
#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix. If the inverses already been calculated, then the cachesolve
#' should retrieve the inverse from the cache.
#' 
#' @param x special matrix
#' @param ... 
#' 
cacheSolve <- function(x, ...) {
    
    # assign an existing inverse matrix
    inv <- x$getInverse()
    
    # checks if inverse matrix exists
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if not - assign matrix to var  
    mat <- x$get()
    
    # inverse matrix
    inv <- solve(mat, ...)
    
    # sets inverse matrix
    x$setInverse(inv)
    
    #returns the inverse matrix
    inv
}

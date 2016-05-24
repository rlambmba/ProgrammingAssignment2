#' 
#' This function creates a special "matrix" object that can cache its inverse
#' 
#' @param x the matrix to be cached
#' 
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()
        x
    setInverse <- function(inverse)
        inv <<- inverse
    
    # returns
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

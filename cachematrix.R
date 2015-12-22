## 2 functions makeCacheMatrix() and cacheSolve()

## makeCacheMatrix() take a matrix as argument, and create 4 functions for this argument
##    set : save the matrix in cache in environnement
##    get : retrieve matrix from environnement
##    setinv : save the inverse of the matrix in cache in environnement
##    getinv : retrieve the inverse of the matrix from environnement

## cacheSolve() return the inverse of a matrix created by makeCacheMatrix()
##    the first call to cacheSolve() compute the inverse, put it in cache and return the result
##    next calls, for the same matrix, return the result from cache

makeCacheMatrix <- function(x = matrix()) {
        ## Create cached environnement for the matrix passed in argument
        matrice <- NULL
        set <- function(y) {
                x <<- y
                matrice <<- NULL
        }
        get <- function() x
        setinv <- function(solve) matrice <<- solve
        getinv <- function() matrice
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)      
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrice <- x$getinv()
        if(!is.null(matrice)) {
                message("getting cached data")
                return(matrice)
        }
        data <- x$get()
        matrice <- solve(data, ...)
        x$setinv(matrice)
        matrice
}

## Josh Gabler R programming Coursera
## The below two functions are used to create a system a caculate that inverse of a matrix
## and store and retreive the results from the cache. This will save computation time as the function
## will not have to re run and rerun over and over again as it it referenced

## makeCacheMatrix:
##this function creates a list of functions with local variables that can be 
##sent to cacheSolve function. The variables are stored in their particular environments

makeCacheMatrix <- function(x = matrix()) {
        ##definition of the 4 functions
        InverseX <- NULL
        set <- function(y) {
                x <<- y
                InverseX <<- NULL
        }
        get <- function() x
        setInv <- function(inv) InverseX <<- inv
        getInv <- function() InverseX
        ## returned list of functions with local variables
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## cacheSolve
## this function takes the list of functions from makeCacheMatrix and will
## compute the inverse of the matrix sent to makeCacheMatrtix, if it has already been created.
## If the inverse has already been created, that is grabbed and returns without recalculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        ## if the inverse has already been calculated, retuns calculated value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##otherwise calculates the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}

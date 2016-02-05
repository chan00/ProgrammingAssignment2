## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory
makeCacheMatrix <- function(x = matrix()) 
{
        # Cache slot in the call environment. Use setinverse to update
        inversed <- NULL
        # Setter for the matrix value. Clears the cache.
        set <-function(y) 
        {
                x <<- y
                inversed <<- NULL
        }
        # Getter for the matrix value
        get <- function() x
        # Setter for the inverted matrix
        setInversed <- function(newInversed) inversed <<- newInversed
        # Getter for the inverted matrix
        getInversed <- function() inversed
        # Create collection of named attributes for the above functions
        list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inversed <- x$getInversed()
        if(!is.null(inversed)) 
        {
                message("getting cached data")
                return(inversed)
        }
        data <- x$get()
        # Calculate the inverse, save in cache, and return
        inversed <- solve(data, ...)
        x$setInversed(inversed)
        inversed
}

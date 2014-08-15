## There are two functions here:
## makeCacheMatrix creates a list of functions to calculate the inverse of the argument x (which has to be an invertible matrix)
## and get/set that value
## cacheSolve is a function that first checks whether the inverse of a matrix has already been calculated. If not, it
## calculates the inverse.

# To run, run all this code (to define the functions).
# Then run out <- makeCacheMatrix(x); cacheSolve(out) where x is your matrix of interest.

## This function creates a list of four functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # Create an empty object, m
        set <- function(y) {
                x <<- y
                m <<- NULL
        } # So actually the code runs perfectly fine without set and I am not sure what it does.
        # I only kept it in because maybe the coursera team runs this code differently than I and the way they do it
        # they do need set.
        get <- function() x
        # get is a function that simply returns x (the matrix fed into the program). So after running this whole code,
        # makeCacheMatrix(x)$get() would run this function and thus return x.
        setinverse <- function(solve) m <<- solve
        # This function takes an object (solve) and redefines m in the higher environment to now be the same as this object (solve).
        # The name solve here could be anything.
        # When running makeCacheMatrix(x)$setinverse(blob) you redefine the object 'm' from makeCacheMatrix(x)
        # Thus if you then call m... it will not be NULL, it will be blob!
        getinverse <- function() m
        # Same as get, this is simply a version to return an object (m in this case). If you have run the function setinverse
        # within the function then m will be blob... and if you have not m will be NULL
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
             # Simply returning the list of functions
}


## This function is going to take makeCacheMatrix, check whether m has a value, and then either return m or
# calculate m (where m is the inverse of the matrix)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        # Run the function getinverse from makeCacheMatrix. As explained this simply returns the object m from the 
        # makeCacheMatrix environment
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # If m from above was already defined then we don't need to calculate the inverse again, m will be the inverse
        # So in that case we simply return m without needing to calculate and we can stop this function
        data <- x$get()
        # If m was NULL, we need to calculate the inverse. This part runs get() from makeCacheMatrix which (as explained)
        # simply returns the original matrix
        m <- solve(data, ...)
        # Now we define m within this function to be the inverse of 'data' (which is the matrix of interest)
        x$setinverse(m)
        # This is the clever part: we have determined that makeCacheMatrix does not contain the inverse yet, so
        # we have calculated the inverse. This line of code then changes the object m in makeCacheMatrix to be the inverse
        # for future reference
        m
        # Finally we return m, which is now the inverse. Next time we run cacheSolve(out), m in out is no longer NULL
        # and thus the m from out will be returned without needing to calculate.
}

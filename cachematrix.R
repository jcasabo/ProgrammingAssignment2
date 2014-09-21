# The following two functions are based in the example mean function of the assignment. 
# They create a matrix that can cache its inverse and returns:
# If the inverse matrix has already been calculated, the inverse matrix
# If the inverse matrix hasn't been calculated, calcules it

# This function creates a list containing four functions:
# set, get, setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
        # First, we set the m value as NULL
        m <- NULL
        # set function sets x to the argument y and set m to NULL
        # in the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get function returns the value of x (i.e., the original matrix)
        get <- function() x
        # setinverse calculate the inverse of x and stores it into m, in the parent environment
        setinverse <- function(solve) m <<- solve
        # getinverse function returns the value of m (i.e., the inverse matrix)
        getinverse <- function() m
        # Now, we create a labeled list containing the 4 functions defined above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # First, we try to get the inverse of x
        m <- x$getinverse()
        # if inverse matrix exists, return a message and the inverse matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # if inverse matrix doesn't exist, create a matrix called "data"...
        data <- x$get()
        # calculate the inverse of "data"
        m <- solve(data, ...)
        # Inverse matrix is stored in the cache. So, we can detect it 
        # when the check for (!is.null(m) , if we wish to run the function again 
        x$setinverse(m)
        # and print m
        m
}

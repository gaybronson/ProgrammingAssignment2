## Functions to create and cache matrix and return inverse of the matrix.  Inverse will not be recalculated if cached in m.

##Function to (i) set initial inverse value, (ii) create the functions used in cacheinverse and (iii) return a list of the functions

makeCacheMatrix <- function(x = matrix()) { ## Establish x as matrix, which will store the inverse
    m <- NULL  ##set initial value of inverse(m) in the local environment
    set <- function(y) { ## establish set as a function that can be used to reset the inverse if a new matrix(x) is run.  Will search and set values of x and m in the parent environment
        x <<- y  ## assigns new matrix values to x and stores them in the parent environment
        m <<- NULL #resets inverse to NULL (cacheSolve will therefore re-run the inverse)
    }
    get <- function() x ## establish get as a function of x; used in cacheSolve to retrieve current matrix data
    setinverse <- function(solve) ## establish setinverse as a function; used in cacheSolve to set value of inverse and store in m
    m <<- solve ## function to store inverse of matrix(x) in m
    getinverse <- function() m #establish function to retrieve cached inverse
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
    ##list of functions used in cacheinverse, containing initial value of inverse (set at NULL)
}

## Function to either retrieve cached inverse or caculate it based on new matrix data

cacheinverse <- function(x, ...) {   # the input x is the list of functions created by makeCacheMatrix
    m <- x$getinverse()               # accesses the object 'x' and gets the value of the inverse (will be NULL if new matrix data used)
    if(!is.null(m)) {              # if inverse was already cached (not NULL) ...
        
        message("getting cached data")  # ... send this message to the console while cached inverse retrieved
        return(m)                       # ... and return the inverse ... "return" ends
    }
    data <- x$get()        # Note: we reach this code only if x$getinverse () returned NULL
    m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse
    x$setinverse(m)           # store the calculated inverse in x (see setinverse() in makeCacheMatrix
    m               # return the newly calculated inverse
}


# This function will create a specical matrix with the ability to cache itself for use in functions that are 
# computationally costly. 
# This function takes a matrix as its input.
# This function returns a list of functions to  get and set the matrix and the inverse of the matrix as follows:-
#
# set(y) : Store a matrix y
# get() : Retrieve the stored matrix
# set.invert(y) : Store an inverted matrix y
# get.invert() : Retrieve the inverted matrix



makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL  # initialize varialbe to store inverted matrix.
    
    
    # Function set() : Store a matrix y
    set <- function(y) {  
        x <<- y                            # x is set to a new matrix.
        inverted <<- NULL                 # Reset inverted variable back to NULL since we have a new matrix.
    }
    
    
    # Function get() : Retrieve the stored matrix
    get <- function() x                             
    
    # Function set.invert(y) : Store an inverted matrix y    
    set.invert <- function(y) inverted <<- y        
    
    # Function get.invert() : Retrieve the inverted matrix
    get.invert <- function() inverted               
    
    
    # Return list of functions for getting and setting the matrix and the inverted matrix.
    list(set = set, get = get,
         set.invert = set.invert,
         get.invert = get.invert)
    
}


# This function will take a special matrix (created by makeCacheMatrix function) and return the inverse of that matrix.
# After the inverse of the matrix is calculated, the inverse is cached. 
# If there is no cached inverse already, then the inverse will be calculated. 
# If the inverse is already cached, then the inverse is not calculated again. Instead the cached result is returned. 

cacheSolve <- function(x, ...) {
    
    i <- x$get.invert()        # Get the current inverted matrix 
    if(!is.null(i)) {          # Check if the matrix is already inverted?
        message("Matrix already inverted. Retrieving cached data...")
        return(i)              # Return the inverted matrix from the cache.
    }
    
    
    # We get to here if the inverted matrix is not already cached.
    message("Matrix not already inverted. Calculating the inverse...")
    data <- x$get()            # Get the matrix
    
    i <- solve(data)           # Get the inverse of the matix.
    x$set.invert(i)            # Set the inverted matrix in the cache.
    i                          # Return the inverted matrix.
    
}

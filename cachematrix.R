## TWO FUNCTIONS for:
## 1. Creating the inverse of a matrix
## 2. Caching the inverse matrix
## 3. Checking to for an existing inverse in the cache before creating the 
##      inverse of a matrix

## First function: makeCacheMatrix
## Input: a matrix - either an existing matrix variable or matrix()
## Output: a list containing 4 functions: get, set, get.inverse, set.inverse
## Example: my.matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## my.matrix$get() - returns matrix created by matrix(1:4, 2, 2)
## my.matrix$set(new.matrix) - sets my.matrix to the same as new.matrix
## my.matrix$get.inverse - returns the inverse of my.matrix
## my.matrix$set.inverse - used by cacheSolve (see below) to save the inverse
##                         of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse.x <- NULL # sets initial inverse of x to NULL
    
    set <- function(y) {
        x <<- y # allows matrix x to be updated by x$set(new matrix)
        inverse.x <<- NULL # resets any cached inverse of x to NULL
    }
    
    get <- function() x # allows matrix to be retreived by x$get()
    set.inverse <- function(inverse) inverse.x <<- inverse # allows inverse
                    # of matrix to be set by x$set.inverse(x) 
                    # called by cacheSolve
    get.inverse <- function() inverse.x # allows previously calculated inverse
                    # of matrix to be retreived by x$get.inverse()
    
    list(set = set,
         get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse) # list of functions to be returned
}


## Second function: cacheSolve
## Input: a list created by the makeCacheMatrix function above which 
##        contains a matrix
## Outputs: the inverse of the matrix retreived from the cache, if it exists,
##          along with a message to that effect
##             --- OR ---
##          the inverse of the matrix is calculated, the inverse is saved in
##          the input list
## Example: my.matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##          my.matrix$get.inverse - returns NULL
##          cacheSolve(my.matrix) - returns inverse of my.matrix
##          my.matrix$get.inverse - now returns inverse of my.matrix

cacheSolve <- function(x, ...) {
    inverse.x <- x$get.inverse() # retreives the inverse of the matrix that is
                                 # currently stored in the cache
    
    if(!is.null(inverse.x)) { # if an inverse matrix exists
        message("getting cached data") # notify user
        return(inverse.x) # return cached inverse
    }
    
    data <- x$get() # retreives the matrix
    inverse.x <- solve(data, ...) # calculates the inverse of the matrix
    x$set.inverse(inverse.x) # saves the inverse to the cache
    inverse.x # returns inverse
}
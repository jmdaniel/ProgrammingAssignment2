## ###########################################
## 
## January 2016
##
## Author: xxxxxx
##
## Module: R Programming, Programming Assigment 2
##
## Functions: 
##   - makeCacheMatrix: Create a special "matrix" object that can cache a matrix
##                      and its inverse.
##                      Return the list of 'setters' & 'getters' to manage that matrix 
##
##   - cacheSolve:      Return the inverted matrix, either from the previous cache,   
##                      or as a new computed matrix, which has now been cached for
##                      future use.
##
## ###########################################

##
## Objective: Create a special "matrix" object that can cache its inverse.
##            This is done by defining a set of functions to manage the matrix 
##            and the corresponding inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
    # Args:
    #   x: square invertible matrix, as per assignment. As such, we assume that Solve() will 
    #      always return the matrix inverse, without error.
    # Returns:
    #   list of setters & getters to manage the matrix and its inverse.
    
    # matInv is the cached inverted matrix
    matInv <- NULL
    
    # Cache the matrix 'x'
    setLocal <- function(y) {
      x <<- y
      matInv <<- NULL
    }
    # Return the cached original matrix
    getLocal <- function() {
      x
    }
    # Cache the inverted matrix in 'matInv'
    setInvLocal <- function(invert) {
      # Assign inverted matrix to cache matrix "matInv"
      matInv <<- invert
    }
    # Return the inverse matrix
    getInvLocal <- function() {
      matInv
    }
    
    # Return the list of all 'setters' and 'getters' functions to manage 
    # the cached matrix: original matrix & inverse matrix.
    list (set = setLocal, get = getLocal, setinv = setInvLocal, getinv = getInvLocal)
}


##
## Objective: Computes the inverse of the special "matrix" returned by the 
##            makeCacheMatrix() function.
##            If the matrix has not changed, and its inverse has already been calculated,
##            the function returns the inverse from the cache.
##
cacheSolve <- function(x, ...) {
    # Args:
    #   x: special "matrix' object, which contains a square invertible matrix, as per assignment. 
    #      As such, we assume that Solve() will always return a correct value, with no error.
    #
    # Returns: matrix that is the inverse of 'x'
  
    # Get the inverse matrix of 'x' from the cache using the getinv() function from the list 'x'
    invert <- x$getinv()
    
    # The inverse matrix has been cached
    if (!is.null(invert)) {
        # We return the cached inverse matrix
        message("Getting cached Inverse Matrix")
        return(invert)
    }
    
    # The matrix hasn't been cached - We grab the original matrix 
    mat <- x$get()
    
    # We compute the inverse matrix
    invert <- solve(mat, ...)
    # We cache its value using the setinv() function
    x$setinv(invert)
    
    # We return the inverse matrix 'invert'
    invert
}

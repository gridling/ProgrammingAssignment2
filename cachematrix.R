## Overall description: 
 # Create a square matrix with n dimensions, 
 #   each of the elements is randomly arranged from a sequence 1 to n^2.
 # The square matrix and its inverse matrix are stored within the object 
 #   created by the function "makeCacheMatrix()"

## makeCacheMatrix()
 # Create a n-by-n square matrix object
 #   with each of the elements randomly arranged from 1 to n^2
 # Cache its inverse matrix in a variable named "inv"

makeCacheMatrix <- function(n) {
    
    inv <- NULL
    x = matrix(sample(n^2), n, n)
 
    getmatrix <- function() x 
    setinv <- function(inverted) inv <<- inverted
    getinv <- function() inv
    
    list(getmatrix = getmatrix, 
         setinv = setinv, getinv = getinv)
}


## cacheSolve()
 # Computes the inverse matrix of the square matrix 
 #   which had been created by and stored in the function makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting the cashed inverse matrix")
        return(inv)
    }
    matrix <- x$getmatrix()
    inv <- solve(matrix)
    x$setinv(inv)
    inv
}


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function caches a matrix into a "special matrix"
# It is expected that the matrix inputed be a 
# square inversible matrix otherwise the solve() 
# function will not be able to inverse the matrix
# This "special matrix" is actually a list of 
# seperate functions.

### EXAMPLE OPERATION
# some_matrix <- rbind(c(1,3,3),c(1,4,3),c(1,3,4))
# special_matrix <- makeCacheMatrix(some_matrix)
### caches a matrix into a "special matrix"
# special_matrix$getmat() 
### will return the cached matrix.
# special_matrix$setinv() 
### will set the cached inverse matrix in
### cacheSolve(special_matrix)
# special_matrix$getinv() 
### will return the cached inverse only if 
### cacheSolve(special_matrix)
### has been ran otherwise it will return NULL.

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    getmat <- function() x
    setinv <- function(inverse) invmat <<- inverse
    getinv <- function() invmat
    list(getmat = getmat,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

# This function returns the inverse of a cached matrix.
# The matrix must be a square "inverseable" matrix.
# If the inverse of the matrix is already cached then the
# cached inverse is returned.

### EXAMPLE OPERATION
### A "special matrix" should have first been created
### using akeCacheMatrix on some square matrix and  
### passing the result to a variable.
# cacheSolve(special_matrix)
### if the x$getinv() isn't NULL
### then the function explictly returns the cached 
### inverse matrix from the "special matrix"
### and messages the string "getting cached inverse"
### Otherwise, the cached matrix is gathered from
### the special matrix's function getmat()
### data <- x$getmat()
### and then the inverse matrix is calcuated using
### R's bulit-in solve() function which returns an
### inverse matrix when an inverseble matrix is 
### given.
### This inversed matrix is then cached into the 
### special matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinv()
    if(!is.null(invmat)) {
        message("getting cached inverse")
        return(invmat)
    }
    data <- x$getmat()
    invmat <- solve(data)
    x$setinv(invmat)
    invmat
}



### SOME TEST CODE
# source("cachematrix.R")
# some_matrix <- rbind(c(1,3,3),c(1,4,3),c(1,3,4))
# a_matrix <- makeCacheMatrix(some_matrix)
# cacheSolve(a_matrix)

### OUTPUT SHOULD BE INVERSE
#      [,1] [,2] [,3]
#[1,]    7   -3   -3
#[2,]   -1    1    0
#[3,]   -1    0    1

###################################################################################################################
##  ASSIGNMENT  : Write a pair of functions that cache the inverse of a matrix
##  FUNCTION1   : makeCacheMatrix
##    PURPOSE   : create a special "matrix" object that can cache its inverse
##    ARGUMENT  : x of type matrix 
##  
##  FUNCTION2   : cacheSolve
##    PURPOSE   : computes the inverse of the special "matrix" object returned by the function makeCacheMatrix
##    ARGUMENTS : x and an ellipsis 
##  EXECUTION (example)
##  > m<- matrix(c(-2,-4,3,1,0,5,3,5,2),3,3)
##  > m
##       [,1] [,2] [,3]
##  [1,]   -2    1    3
##  [2,]   -4    0    5
##  [3,]    3    5    2
##  > m1 <- makeCacheMatrix(m)
##  > m1$getmat()
##       [,1] [,2] [,3]
##  [1,]   -2    1    3
##  [2,]   -4    0    5
##  [3,]    3    5    2
##  > m1$getinv()
##  NULL
##  > inv <-cacheSolve(m1)
##  > inv
##            [,1] [,2]       [,3]
##  [1,] -1.923077    1  0.3846154
##  [2,]  1.769231   -1 -0.1538462
##  [3,] -1.538462    1  0.3076923
##  > m1$getinv()
##            [,1] [,2]       [,3]
##  [1,] -1.923077    1  0.3846154
##  [2,]  1.769231   -1 -0.1538462
##  [3,] -1.538462    1  0.3076923
##  > inv <-cacheSolve(m1)
##  getting the inverse of the matrix from the cache

###################################################################################################################

## The function makeCacheMatrix creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL                         ## Initializes matinv as NULL - matinv will hold the inverse of the matrix
        setmat <- function(y) {                ## function setmat: argument y
                x <<- y                        ## Assigns y to the x object in the parent environment
                matinv <- NULL                 ## Initializes matinv in the parent environment, value of matinv in cache is cleared
        }
        
        getmat <- function() x                 ## function getmat: no argument, takes the matrix x from the parent environment
        
        setinv <- function(inv) matinv <<- inv ## function setinv: argument inv, assigns inv to mativ in the parent environment
        
        getinv <- function() matinv            ## function getinv: no argument, takes matinv from the parent environment
        
        list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv) ## Creates a list with the previous functions as elements 
                                                                                 ## and assigns a name to each function
}

## The function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache 
## instead of recalculating it
cacheSolve <- function(x, ...) {
        matinv <- x$getinv()                  ## Retrieves from the cache the inverse of the matrix passed in as argument in matinv
        if(!is.null(matinv)) {                ## If matinv is not NULL, 
                                              ## it means that the inverse of the matrix is already stored in the cache
                message("getting the inverse of the matrix from the cache")
                return(matinv)                ## the matrix is returned from the cache
        }

        matori <- x$getmat()
        matinv <- solve(matori, ...)          ## If the inverse of the matrix is not yet cached, 
                                              ## the inverse of the matrix is calculated using the solve fonction
        x$setinv(matinv)                      ## Sets the inverse of the matrix 
        matinv                                ## Retunrs the inverse of the matrix
        
}

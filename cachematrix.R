## These functions work together to create a cached inverse of a given matrix such
## that if the inverse of a matrix is created once, subsequent calls to compute
## its inverse will return the inverse stored in cache rather than re-computing.

## Create the functions necessary to store and retrieve a matrix and its inverse.
## Leverage R's scoping rules to set values defined outside a function's context.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)}


## Create the inverse of the specified matrix if it has not been created already. If the inverse has not been
## created, so do, then cache the results. In either case, return the inverse of the specified matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
}

## Create a test case for the above.
##  (1) Create one invertible matrix with caching capabilities (makeCacheMatrix)
##  (2) Compute its inverse (cacheSolve)
##  (3) Compute its inverse again (cacheSolve)

## Expected Results:

## The first time through cacheSolve, the message 'getting cached data' should not appear, since, 
## in fact, the matrix's inverse has not yet been cached. The second pass should produce the 
## 'getting cached data' messsage, since the first pass cached the results.

testCache <- function(){
    matrix1 <- matrix(c(2,2,1,2), nrow=2)
    print (matrix1)
    matrix2 <- matrix(c(3,3,3,3), nrow=2) 
    print (matrix2)
    cacheMatrix1 <- makeCacheMatrix(matrix1)
    message ('First time through : matrix 1 inverse')
    matrix1Inverse <-cacheSolve(cacheMatrix1)    
    print (matrix1Inverse)
    message ('Second time through : matrix 1 inverse')
    matrix1Inverse <-cacheSolve(cacheMatrix1)    
    print (matrix1Inverse)
}

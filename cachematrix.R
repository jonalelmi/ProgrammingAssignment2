## We create two functions that, combined together, allow to cache the inverse
## of an invertible matrix.

## The first function, makeCacheMatrix, allows to construct, given a matrix, a list
## of functions containing
## 1. a function 'set' to set the value of the matrix
## 2. a function 'get' to get the value of the matrix
## 3. a function 'setinverse' to specify the inverse of the matrix
## 4. a funtion 'getinverse' to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(A){
        x <<- A
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(INV) inv <<- INV
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


##  The second function, chacheSolve, allows to compute the inverse of the matrix.
##  First, the function checks whether a cached value of the inverse exists. If this is not the case
## the function evaluates the inverse using solve() and assigns the result using x$setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        print('Using cached inverse')
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    x
}

## The following pair of functions will cache the inverse of a matrix

## The "makeCacheMatrix" will create a matrix object that can cache 
## the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(matrix) {
          mat <<- matrix
          inv <<- null
        }
        get <- function() {
          mat  
        }
        setinverse <- function(inverse) {
          inv <<- inverse 
        }
        getinverse <- function() {
          inv
        }
        list(set = set
             , get = get
             , setinverse = setinverse
             , getinverse = getinverse)
}


## The "cacheSolve" function calculates the inverse of the vector created with  
## the "makeCacheMatrix" function. If the inverse has already been calculated
## this function gets the inverse from the cache and skips the calculation. 
## However, if the inverse has not already been calculated then the inverse 
## of the data IS calculated and is set as the value of the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        if(!isnull(mat)) {
            message("getting cached data")
            return(mat)
        }
        data <- x$get()
        mat <- solve(data) %*% data
        x$setinverse(mat)
        mat
}

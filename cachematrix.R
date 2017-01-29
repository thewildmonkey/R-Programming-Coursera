## Week 3 Programming Assignment 2:  Write a pair of functions that cache 
## the inverse of a matrix. 

## This function creates a special "matrix" object that can 
   ## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrixinv <- function(solve) m <<- solve
        getmatrixinv <- function() m
        list(set = set, get = get,
             setmatrixinv = setmatrixinv,
             getmatrixinv = getmatrixinv)
}


## This function computes the inverse of the special "matrix" 
   ## returned by makeCacheMatrix above. If the inverse has already 
   ## been calculated (and the matrix has not changed), then 
   ## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrixinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrixinv(m)
        m
}

matrix1<-c(2,3,2,2)
dim(matrix1)<-c(2,2)
matrix1
solve(matrix1)


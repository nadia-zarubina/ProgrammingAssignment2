## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ###set the value of the matrix
        k <- NULL
        set <- function(y) {
                x <<- y
                k <<- NULL
        }
        ### get the value of the matrix
        get <- function() x
        
        ### set the inverse of the matrix
        setinverse <- function(solve) k <<- solve
        getinverse <- function() k
        
        ### get the inverse of the matrix
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ### get the inverse of the matrix
        k <- x$getinverse()
        
        ### check if there is the matrix
        if(!is.null(k)) {
                message("getting cached data")
                return(k)
        }
        
        ### if not, get the inverse of the matrix
        data <- x$get()
        k <- solve(data, ...)
        
        ### set the inverse of the matrix
        x$setinverse(k)
        k
}


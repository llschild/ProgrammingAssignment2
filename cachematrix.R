## Function description as follows:
## makeCacheMatrix returns a list as a represtation of the special matrix.
## The list contains 4 parameters. 
## "set" resests the matrix ,
## "get" returns the matrix, 
## "setinverse" stores the inverse
## "getinverse" returns the cached inverse if present.

        makeCacheMatrix <- function(x = matrix()) {
                inv= NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }
        
        
## This function takes the list from makeCacheMatrix 
## and checks if it contains an inverse.
## If the inverse is not NULL the function returns it. 
## If it is NULL, it calculates, stores and returns it.
        
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

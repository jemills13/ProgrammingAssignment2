## Put comments here that give an overall description of what your
## functions do

# When running time consuming computations,
# it can save time and energy to catche functions and look them up again, rather than computing them again.  
# The following functions can catche, and store the value of a function


## Write a short comment describing this function
# This functioncreats a matrix object than can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invs <<- inverse()
        getinverse <- function() inv
        list(set = set, get=get, setinverse=setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# Computes the inverse of the "matrix" from the MakeCacheMatrix function.
# If the inverse matrix has already been calculated, it takes the inverse directly from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs =x$getinvs()
        
        #if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skip the computation
                message("You are getting cached data!")
                return(inv)
        }
        #otherwise, calculate the inverse
        
        mat.data=x$get()
        inv = solve(mat.data,...)
        
        #sets the value of the inverse in the cache via the setinv function
        x$setinverse(inv)
        return(inv)
}

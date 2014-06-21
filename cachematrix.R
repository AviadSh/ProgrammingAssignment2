## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function will return an object that holds a matrix, and if already computed, the inverse matrix.
## $get - returns the matrix
## $set - stores a matrix in the object. this method also nullify the cached copy of the inverse of the matrix.
## $setinverse - stores the inverse matrix
## $getinverse - returns the inverse matrix
##
## this function creates two variables in the parent frame, called x, inv - which are part of the object, and functions that are part of the object as well
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse2) inv <<- inverse2
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## this function receives an object of type makeCacheMatrix, and returns the inverse matrix of the matrix stored in it.
## first it'll check if the inverse matrix is stored in the cache variable
## if so, it'll return it.
## if not, it'll compute the inverse matrix, store it in the cache and return it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## try to get the inverse matrix from the cache of the object.
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		## meaning that the cache was empty, so let's get the matrix
        data <- x$get()
		## calculate the inverse matrix
        inv <- solve(data, ...)
		## store the inverse matrix in the cache
        x$setinverse(inv)
		## return the result
        inv
}

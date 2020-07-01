## Put comments here that give an overall description of what your
## functions do

## the function 'makeCacheMatrix', will produce the inverse of a matrix 
## the function 'cacheSolve' creates the inverse of the matrix found by
## 'makeCacheMatrix', if the inverse has not yet been caclulated it will
##compute the value of the matrix and create the cache inverse.

## Write a short comment describing this function
## assume the function is inverse and give it the value NULL, 
##then set the value of the matrix and get the value of the matrix
##then set the value of the Inverse and get the value of the Inverse 
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL 
set <- function(y){
        x <<-y
        inv <<- NULL
}
get <- function() (x)
setInverse <- function(inverse) (inv<<- inverse)
getInverse <- function() (inv)
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## first line returns a matrix that is inverse of "x"
##if the inverse is retrieved from the cache then produce the message,
## otherwise compute the value of the matrix and set the value of the cache inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

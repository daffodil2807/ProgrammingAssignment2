## Create a list using makeCacheMatrix and assign the following function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function()inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##  This function is exclusively  valid only for inverse matrix.
# If output is inverse it will give result.
# Else, it computes the inverse, sets the value in the cache via
# setinverse function.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
          if(!is.null(inv)) {
       message("getting cached data.")
               return(inv)
          }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
## Example for the assignment
x = rbind(c(2,-4), c(-4, 2))## creation of matrix
m = makeCacheMatrix(x) ## define variable for command
m$get() ## run output command

cacheSolve(m) ## this command will work only if inverse matrix


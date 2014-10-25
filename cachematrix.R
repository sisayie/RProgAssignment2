#Assignement 2
# Create a matrix
# get result, if cache exists or
# calculate its inverse
# cache the result
makeCacheMatrix <- function(x = matrix()) {
# inv will store the cached inverse matrix
inv <- NULL
# 1. Set the value of the matrix
setmatrix <- function(y) {
x <<- y
inv <<- NULL
}
# 2. Get the value of the matrix
getmatrix <- function() x
# 3. Set the value of the inverse
setinverse <- function(inverse) inv <<- inverse
# 4. Get the value of the inverse
getinverse <- function() inv
# Return the matrix with our newly defined functions
list(set = setmatrix, get = getmatrix, setinv = setinverse, getinv = getinverse)
}
# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
inv <- x$getinverse()
# If the inverse is already calculated, return it
if (!is.null(inv)) {
message("getting cached data")
return(inv)
}
# The cache does not exist, perform new calculation
data <- x$getinverse()
inv <- solve(data, ...)
# Cache the inverse
x$setinverse(inv)
# Return inverse
inv
}

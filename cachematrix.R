## this function is used to make a cache for vector inverses
## this function returns a list of functions including:
## 1. setMatrix that takes in a vector and assigns it to x, and assigns the inverse 'i' as null
## 2. getMatrix that outputs the currently assigned matrix
## 3. setInverse that sets the function argument to i
## 4. getInverse that outputs the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setMatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## this function is used to calculate the inverse of a matrix which is assigned to a list using the setMatrix function
## in makeCacheMatrix
## the function first checks to see if an inverse was previously calculated by applying the getInverse function
## if the getInverse function returns a null value then a solve operation is completed with the inversed matrix set to
## 'i' using the setInverse function in makeCacheMatrix
## if a previous inverse exists then the a message "getting cached data" is displayed along with the cached inversed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}

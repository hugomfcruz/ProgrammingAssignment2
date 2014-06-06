## This function creates an object that contain the matrix and,
## if calculated, the inverse of that matrix
## It is implemented as a list with 4 methods:
## get() - returns the original matrix
## set(y) - sets the original matrix
## getinverse() - returns the inverted matrix
## setinverse(y) - sets the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
        set <- function(y) {
                x <<- y
                ##if matrix is changing, invalidate the cache
                inverse <<- NULL
        }
        get <- function() { x }
        setinverse <- function(inverted){ inverse <<- inverted }
        getinverse <- function(){ inverse }
        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverted matrix
## If the inverted matrix was calculated before, a cached copy is returned
## Otherwise, the inverse in calculated and cached
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ##Calculate the inverse of the matrix
        data <- x$get()
        inverse <- solve(data)
        #Add the calculated matrix to the cache
        x$setinverse(inverse)
        inverse
}

## testing
#originalmatrix <- matrix(c(4, 2, 7, 6), nrow=2, ncol=2)
#mymatrix <- makeCacheMatrix(originalmatrix)
#cacheSolve(mymatrix)
#cacheSolve(mymatrix)
##change mymatrix
#mymatrix$set(matrix(c(2, 2, 3, 2), nrow=2, ncol=2))
#mymatrix <- cacheSolve(mymatrix)
#mymatrix <- cacheSolve(mymatrix)

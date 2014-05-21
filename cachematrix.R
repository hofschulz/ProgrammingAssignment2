#This pair of functions will create a cache to store a matrix. It is similar
# to a class in object oriented programming. There are methods within the 'makeCacheMatrix'
# that allow for storing and retrieving a matrix and its inverse.

#Example use:
#> source("cachematrix.R")
#> folly <- matrix(c(1,4,5,2,3,7,9,2,1),3,3)
#> makeCacheMatrix(folly)       
#> joke <- makeCacheMatrix(folly)
#> cacheSolve(joke)
#> cacheSolve(joke) 
### NOTE: after the second call to cacheSolve(joke) the console
### should display the message 'getting cached data'
#> joke$get()
#> joke$getinverse()
#This function contains the initialization and accessor methods for the matrix
#that will be cached. It can set a matrix, retrieve the matrix, set an inverse matrix, 
#retrieve an inverse matrix. The solve() function is used with matrix in order
#to take advantage of built-in efficiency. It assumes the matrix is a square matrix.
makeCacheMatrix <- function(x = matrix()) {
#check that the matrix has equal dimensions
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#Once it is stored in the cache, operations can be performed on it.
# Additionally, the next time the matrix is used, the cached version 
# will be accessed, and the matrix retrieved from there
#The purpose of this function is to determine if the inverse of a matrix has been
# calculated and stored already. If it has not, the cacheSolve function calls the
# setinverse() method and copies that result into the cache, m. If it has, then the
# getinverse() method retrieves the inverse from the cache, m. If it is from 
# the cache, then a message prints to the console to indicate such.

#The matrix will be accessed from the cache rather than using resources 
#to build another -- provided the matrix has been loaded in the cache
#It is assumed that the matrix is a square matrix, as stated in the assignment,
# in order for the solve() function to work correctly. No checks are attempted.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

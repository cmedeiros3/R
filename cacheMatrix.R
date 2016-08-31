## This file presents an different way to obtain the inverse of a matrix.
## Rather than calculate the inverse every time that it ask, it allows to store the results in 
## cache, and recover it when necessary, avoiding to calculate the same inverse again.
## Here an example:
## > mymatrix <- matrix(1:9,nrow=3,ncol=3)
## > mymatrix
## > mycache <- makeCacheMatrix(mymatrix)
## > cacheSolve(mycache)
## Ready, your inverse is in cache, if you apply this funciont again:
## > cacheSolve(mycache)
## [,1] [,2] [,3]
## [1,] -3.4  2.8 -0.4
## [2,] -0.2  0.4 -0.2
## [3,]  2.6 -2.2  0.6
## you going to get the message showing that the value was get in cache
## getting cached data
## [,1] [,2] [,3]
## [1,] -3.4  2.8 -0.4
## [2,] -0.2  0.4 -0.2
## [3,]  2.6 -2.2  0.6

## makeCacheMatrix is a function to define set and get properties
## that allows to recover the matrix value when it is in cache
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve is a function to calculte the inverse value of a matrix, that apply the solve()
## function only if the inverse matrix isn't in cache, otherwise the function just get the value
## that is already in cache, causing the result to be obtained faster.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}


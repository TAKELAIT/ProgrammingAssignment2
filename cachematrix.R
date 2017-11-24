## Fouzi TAKELAIT, 2017-11-24
## cachmatrix.R
## Example Simulation:
## assume that the matrix supplied is always square invertible matrix where det(invertibleMatrix) != 0
## invertibleMatrix <- matrix(c(1,0,5,2,1,6,3,4,0), ncol = 3)
## cachInversMatrix <- makeCacheMatrix(invertibleMatrix)
## cachInversMatrix$getMatrix() ## returns:
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
## cachInversMatrix$getCache() ## returns NULL for its 1st execution
## cacheSolve(cachInversMatrix) ## will return the inverse of the matrix 'invertibleMatrix'
## cachInversMatrix$getCache() ## this time returns the inverse of the matrix 'invertibleMatrix'
#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # 1. initialize the cache Matrix 'cacheMatrix' object
        # assign the value NULL for the first initialization
        
        cacheMatrix <- NULL
        
        # 2. define the method named 'setMatrix'
        
        setMatrix <- function(y) {
                x <<- y
                cacheMatrix <<- NULL
        }
        
        # 3. define the method named 'getMatrix'
        # return the matrix 'x'
        
        getMatrix <- function() x
        
        # 4. define the method named 'setCache'
        
        setCache <- function(inverse) cacheMatrix <<- inverse
        
        # 5. define the method named 'getCache'
        # that will return the cached inverse of 'x'
        
        getCache <- function() cacheMatrix
        
        # 6. list the names of all methods that will be known to the outside world
        # although the name in this list can be different from the name of the methods defined above
        # I choose the same name as the methods defined above for simplicity and coherence
        
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setCache = setCache,
             getCache = getCache)
}


## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above (so this is from the cache). 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # 1. check the content of cache matrix
        
        cacheMatrix <- x$getCache()
        
        # 2. if the content is not null then: return the result 
        
        if (!is.null(cacheMatrix)) {
                message("loading cache matrix...")
                return(cacheMatrix)
        }
        
        # 3. if the content is empty then: 
        # get the matrix, create, set, update and return the cache matrix
        else {
                Matrix <- x$getMatrix()
                cacheMatrix <- solve(Matrix, ...)
                x$setCache(cacheMatrix)
                return(cacheMatrix)
        }
}
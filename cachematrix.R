## The function gets a matrix and return its invert
## creates an R object that stores a matrix and its invert 

## First function is makeCacheMatrix which creates an object in 
## the parent environment of the function. The matrix, its inverse and
## four other functions are recorded which they can be called exrternally.





makeCacheMatrix <- function(x = matrix()) { ## Input of the function is x
        m <- NULL             ## NULL is assigned to m here
        set <- function(y) {
                x <<- y
                m <<- NULL  # again similar initialisation done in case 'set' is called to change the matrix stored in
        }
        get <- function() x
        setinverse <- function(inv) m <<- inverse   ##Set the value of the matrix's inverse in m
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
                
## cachmean calculates the inverse of the mtrix x
## If it already exists in cache, it will report "getting cached data" and print the inverse
## Otherwise the inverse is reported 

cacheSolve <- function(x, ...) {
        m <- x$getinverse() ## Here the inverse of the matrix is retrieved
        if(!is.null(m)) {
                ## Here we check if the inverse of the matrix
                ## exist in cache. If it is not Null then the same value is returned
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)   ## This function calculates the inverse of the matrix. Note here we assume the matrix is invertible.
        x$setinverse(m)         ## This function set the value of inverse
        m                       ## Here Inverse is reported
}

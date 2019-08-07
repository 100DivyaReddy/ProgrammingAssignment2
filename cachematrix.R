## Functions that create an inverse matrix and cache it

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## initializing the inverse property
    
    ## function to set matrix
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    
    ## function to get the matrix
    get <- function()
    {
        m
    }
    
    ## function to set the inverse of the matrix
    setInverse <- function(inverse){
        i <<- inverse
    }
    
    ##function that returns the inverse matrix
    getInverse <- function(){
        i
    }
    
    ## return the methods
    list(set = set, get = get, 
         setInverse = setInverse, getInverse= getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## If its already set then just return inverse
    if(!is.null(m)){
        message("Fetching cached data")
        return(m)
    }
    
    ## Get the matrix object and calculate the inverse using
    ## matrix multiplication, set the inverse and return matrix
    
    data <- x$get()
    
    m <- solve(data) %*% data
        
    x$setInverse(m)
    
    m
    
}

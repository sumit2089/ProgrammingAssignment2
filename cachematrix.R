## The function makeCacheMatrix will make a special matrix that will be 
## able to cache its inverse
## The function cacheSolve will compute the inverse of the matrix, 
## after checking that the inverse does not already exist in the cache

## makeCacheMatrix is the function to create a special matrix which can save
## the inverse of the matrix in cache
makeCacheMatrix <- function(x = matrix()) {
    
    ## initializing the inverseMatrix to NULL
    inverseMatrix <- NULL    
    
    ## set is the function used to set the value of the  matrix 'x' 
    ## to 'y' as passed to set function
    set <- function(y) {                 
        x <<- y                 
        inverseMatrix <<- NULL         
    }  
    
    ## get() is the function used to get the value of the matrix 'x' 
    get <- function() x    
    
    ##setInverse is a function used to set the inverse 
    ## value, as passed to setInverse(), for the given matrix in cache
    setInverse <- function(inv) inverseMatrix <<- inv   
    
    ##getInverse is a function used to get the inverse 
    ## value for the given matrix from the cache
    getInverse <- function() inverseMatrix         
    list(set = set, get = get,              
         setInverse = setInverse,              
         getInverse = getInverse) 
}


## cacheSolve is the function to compute the inverse of the matrix
cacheSolve <- function(x, ...) {
    
    ## For the special matrix 'x', the inverse is looked for in cache
    inverseMatrix <- x$getInverse()    
    
    ## If the inverse matrix is found, then the inverse matrix is returned
    if(!is.null(inverseMatrix)) {                 
        message("Inverse Matrix found in cache! Getting cached data...")                 
        return(inverseMatrix)         
    }
    
    ## If not found, then the value of the matrix 'x' is fetched using get()
    matData <- x$get()      
    
    ## The inverse is computed with solve()
    inverseMatrix <- solve(matData)    
    
    ## The computed inverse is saved in cache with setInverse
    x$setInverse(inverseMatrix)       
    
    ## The computed inverse is returned
    inverseMatrix
}

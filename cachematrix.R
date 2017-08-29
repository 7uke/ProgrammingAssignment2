## Function creates special matrix from x and store it's values
## (x - original matrix; i - inversed matrix) and functions 
## (getters and setters)
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL               ## initializing i with NULL value
        
        set <- function(y) {
                x <<- y         ## this function enables changing object
                                ## without initializing another instance
                                ## It is unnecessary the first time an object of 
                                ## type makeCacheMatrix is instantiated
                i <<- NULL      ## clears any value of m that had been
                                ## cached by a prior execution of cacheSolve()
        } 
        
        get <- function() x     ## defines getter of matrix x
        
        setinv <- function(inv) i <<- inv ## setter for inversed matrix
        
        getinv <- function() i  ## getter for inversed matrix
        
        list(get=get, set=set,  
             getinv=getinv, setinv=setinv) 
                                ## assigns all the elements within a list()
}


## CacheSolve is required to populate or retrive the value 
##(inverted matrix) from an object of type makeCacheMatrix
cacheSolve <- function(x, ...) {
        i <- x$getinv()         ## attempt to retrieve a value from the 
                                ## object passed in function's argument
        if (!is.null(i)) {      ## if the above is not NULL, retrieves it..
                message("Cached value of matrix")
                return(i)
        }
        
        
        data <- x$get()         ##..else assign the passed object to data
        i <- solve(data, ...)   ## and inverse it,
        x$setinv(i)             ## and sets the new value in special our matrix        
        return(i)
}

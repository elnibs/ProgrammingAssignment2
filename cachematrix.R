## Put comments here that give an overall description of what your
## functions do

## This function is responsible for providing functions
## to manipulate a matrix's inverse
## 
## returns a list of four elements:
## 1- set : sets the matrix value
## 2- get : get the matrix
## 3- setinv: sets the inverse value of the matrix
## 4- getinv: gets the inverse value of the matrix (in case it was supposly inversed already)

makeCacheMatrix <- function(x = matrix()) {
    
        # initialize the matrix inverse to be NULL
        inv <- NULL
        
        # matrix setter
        # sets the inverse to null
         set <- function(y) {
                x <<- y
                inv <<- NULL
             }
        
        # matrix getter
         get <- function() x
        
        # inverse setter
         setinv <- function(val) inv <<- val
        
        # inverse getter
         getinv <- function() inv
        
        # makeCacheMatrix's return result
        # a list of the methods provided in order: get, set, setinv, getinv
         list(set = set, get = get,
                         setinv = setinv,
                         getinv = getinv)
}


## Gets the inverse of a provided matrix
## In case the inverse was already calculated
## return the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # get the inverse
    inv <- x$getinv()
    
    # check if the inverse is null if not return it
         if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
             }
    
    # inverse is null and thus have to calculate it
         data <- x$get()
         inv <- solve(data)
         x$setinv(inv)
    
    # return the newly calculated inverse matrix
    inv
}

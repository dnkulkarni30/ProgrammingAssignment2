## This is pair functions one to createa a matrix in a way to be used later for caching. The other function 
## then can cache the inverse of same and returns inverse.


## This function creates matrix and returns a list of functions using which we can see the content
## of the matrix and also see the inverse of the matrix(if already calculate)

makeCacheMatrix <- function(x = matrix()) {
        
        invX <- NULL
       
        #function 1 to populate matrix data.
        set <- function(y) 
          {
              x <<- y    
              invX <<- NULL 
          }
        
        #function 2 to get the matrix data  
        get <- function() x
        
        #function 3 to set the inverse of the matrix with supplied values
        setInverse <- function(inv) invX <<- inv 
        
        #function 4 to get the inverse of the matrix in return.
        getInverse <- function() invX
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##This function is to cache the inverse of a matrix and use it whenever call is made to this function.
##It assumes that the matrix passed is a square matrix and is invertible
cacheSolve <- function(x, ...) 
{
        invX <- x$getInverse()
        if(!is.null(invX)) {
          message("getting cached matrix inversion data")
          return(invX)  #return and abort.
        }
        data <- x$get()
        invX <- solve(data)
        x$setInverse(invX)
        invX #return
}


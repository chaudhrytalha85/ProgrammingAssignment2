## The function makeCachematrix below creates a special object that 
## sets a matrix as an argument, and then can cache the inverse of 
## the matrix. The function cacheSolve then calculates the inverse 
## of the matrix that is returned by makeCachematrix, then returns 
## inverse. If the inverse has already been computed beforehand, it
## simply returns the cached object.

## This function creates a special object, that sets a matrix and 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   
   get <- function() x 
   setsolve <- function(solve) m <<- solve
   getsolve <- function() m 
   
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
   
}


## This function computes the inverse of the matrix set in 
## makeCachematrix, computes it inverse and returns it. If it has
## already been calculated earlier, then it simply retrieves it 
## from cache

cacheSolve <- function(x) {
     m <- x$getsolve()
     if(!is.null(m)){
       message("getting cached data")
        return(m)
     }
        
     my_matrix <- x$get()
     m <- solve(my_matrix)
     x$setsolve(m)
     m
  ## Returns a matrix that is the inverse of 'x'
}

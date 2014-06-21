#the makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      
      #create vector to cache the inverse of the "matrix" object
      invr <- NULL
      
      #function to set the "matrix" object
      set <- function(y) {
            x <<- y
            invr <<- NULL
      }
      
      #function to get the "matrix" object
      get <- function() x
      
      #function to set the inverse of the "matrix" object
      setinvr <- function(inverse) invr <<- inverse
      
      #function to get the inverse of the "matrix" object
      getinvr <- function() invr
      
      #return the "matrix" object as defined by the functions previously set
      list(set=set, get=get, setinvr=setinvr, getinvr=getinvr)
}


#the cacheSolve fuction creates a fuction that computes  the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      
      #return the inverse of "x" as a matrix 
      invr <- x$getinvr()
      
      #if statement to determine if the inverse has already been calculated
      if(!is.null(invr)) {
            message("Retrieving cached data.")
            return(invr)
      }
      
      #in the case that the inverse has not been calculated, this statement will caculate it
      data <- x$get()
      invr <- solve(data)
      
      #cache the inverse
      x$setinvr(invr)
      
      #return the matrix defined as an inverse of "x"
      invr
}

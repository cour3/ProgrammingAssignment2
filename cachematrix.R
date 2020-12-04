## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## inline #####

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL ## Stores the inverse
     new<-TRUE  ## flag that remembers if an updated matrix 
                ##is stored that requires calculating the inverse
     set <- function(y) {
          if (!identical(x,y)) new <<- TRUE ## if I set with different value then
                        ## then track that as new = TRUE, else no need to change
                        ## the state of the flag
          x <<- y
          m <<- NULL
          
     }
     get <- function() x
     setinv <- function(invx) m <<- invx
     getinv <- function() m
     setnew <- function(nnew) new <<- nnew  # not actually needed
     getnew <- function() new     
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv,
          setnew = setnew,
          getnew = getnew)
}

## Write a short comment describing this function
## inline

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()
     if(!is.null(m)&&!x$getnew()) {  #if no inverse already calculated or
                     # there was no change of the data (i.e., new == FALSE),
                     #then return cached data
          message("getting cached data")
          return(m)                  
     }
     data <- x$get()  # else set m in the structure to store the new inverse
     m <- solve(data, ...)
     x$setinv(m)
     m
}
## MakeCacheMatrix function creates a matrix object that can cache its inverse.
## sample is the matrix object that user will submit on the console


makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL

#set the value of the Matrix
    setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
    }
      getMatrix <- function() x                              
      setInverse <- function(inverse) invMatrix <<- inverse 
      getInverse <- function() invMatrix                     
      list(setMatrix = setMatrix, getMatrix = getMatrix,
                 setInverse = setInverse, getInverse = getInverse)
      
}


## This function computes the inverse of the matrix created by makecachematrix function
##  If the inverse has already been calculated  then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
    #get the value of the invertible matrix from the makeCacheMatrix function
      invMatrix <- x$getInverse()
    if(!is.null(invMatrix)) {                       
     message("Getting Cached Invertible Matrix")   
    return(invMatrix)                             
   }
      
#if value of the invertible matrix is NULL then  
           MatrixData <- x$getMatrix()                     
       invMatrix <- solve(MatrixData, ...)             
        x$setInverse(invMatrix)  
        ## Return a matrix that is the inverse of 'x'
        return(invMatrix)                               
      
}

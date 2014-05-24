## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # invMatrix use to store the cache inverse matrix
  invMatrix<-NULL
  
  # function to set the matrix
  set<-function(y){
    x<<-y
    invMatrix<-NULL
  }

  ## function to get the matrix
  get<-function() x

  ## function to set the inverse matrix
  setmatrix<-function(matrix) invMatrix<- matrix

  ## function to get the inverse matrix
  getmatrix<-function() m
  
  # Return the set matrix
  list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}



## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
    
    ## get x matrix
    invMatrix<-x$getmatrix()
    
    ## test is invMatrix is null or not
    ## if not null then the matrix was already cache
    ## just print a message and return the matrix without any calculation
    if(!is.null(invMatrix)){
      message("getting cached data")
      return(invMatrix)
    }
    
    ## Here the invMatrix was not in cache
    ## need to calculate the inv matrix
    ## Get the matrix to calculate the inverse
    matrix<-x$get()
    ## Calculate the inverse Matrix thanks to solve function
    invMatrix-solve(matrix, ...)
    
    ## cache the inverse matrix
    x$setmatrix(invMatrix)
    
    ## return the invMatrix
    invMatrix
}

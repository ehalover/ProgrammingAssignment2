## The two functions below, makeCacheMatrix and cacheSolve, are used to create a special object that  
## stores a matrix and caches its inverse.


## This function creates a special "matrix" object that can cache its inverse.It contains four functions to
## set and get the value of the matrix and set and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invrs<-NULL
  set<-function(y){
    x<<-y
    invrs<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) invrs<<-inverse
  getInverse<-function() invrs
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix if it has not been calculated;
## otherwise, the cacheSolve retrieves the inverse from the cache.
## The function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  invrs<-x$getInverse()
  if(!is.null(invrs)){
    message("getting cached data")
    return (invrs)
  }
  data<-x$get()
  invrs<-solve(data,...)
  x$setInverse(invrs)
  invrs
        
}

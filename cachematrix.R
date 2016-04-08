## Inverse Matrix - Programming Assignment 2 

## In the first function, want to set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL 
  }
  get <- function() x
  setinverse <- function (inverse)inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This second function first verifies whether or not the inverse has been computed
##if yes, it caches the inverse. If not, it will calculate the inverse ofthe matrix and set its value using the setinverse function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message( "getting cached data.")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


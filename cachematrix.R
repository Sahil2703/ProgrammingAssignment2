library(MASS)
##This function creates a special "matrix", which is really a list containing a function to-
##set the elements of the matrix, get the elements of the matrix, set the elements of the matrix inverse, get the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL                       #initializing inv as null
  set<-function(y){
    x<<-yinv<<-NULL
  }
  get <- function() x             #function to get matrix x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The following function calculates the inverse of the special "matrix" created with the above function
cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)) {                #check whether inverse is null
    message("getting cached data")
    return(inv)                      #return inverse value
  }
  data<-x$get()
  inv<-solve(data, ...)              #calculate inverse value
  x$setinverse(inv)
  inv
}

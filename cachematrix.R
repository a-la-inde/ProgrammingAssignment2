
library("Matrix", lib.loc="C:/Program Files/R/R-3.1.0/library") 

  
## Write an object 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() 
    x
  setsolve <- function(solve) 
    m <<- solve
  getsolve <- function() 
    m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  }


## 

cacheSolve <- function(x, ...) {
    invx <- x$getsolve()
    if(!is.null(invx)) {
      message("getting cached data")
      return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setsolve(invx)
    invx
  }


## check functions 

m <- matrix(sample.int(100,size=16,replace=TRUE), nrow=4)
x <- makeCacheMatrix(m)
class(x)

cacheSolve(x)

y <- cacheSolve(x)

class(y)

m <- matrix(sample.int(100,size=25,replace=TRUE), nrow=5)
x <- makeCacheMatrix(m)
cacheSolve(x)

cacheSolve(x)

## check time using

system.time(x <- matrix(data=NA,nrow=10000,ncol=10000)) 


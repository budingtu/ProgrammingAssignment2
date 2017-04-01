## the first function creates an object, the second one calculates the inverse

## prepare for the calculation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the value of the matrix
  setinverse<- function(inverse) m <<- inverse #set the value of the inverse
  getinverse <- function() m #get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## get the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { #check if the inverse already exists
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #calculate inverse of the matrix
  x$setinverse(m)
  m
}

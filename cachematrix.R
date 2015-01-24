#  The purpose of this is to create a matrix and inverse it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


 
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## test data
vecData <- rbind(c(1,2),c(2,3))
matrix = makeCacheMatrix(vecData)
print(matrix$get())

print(cacheSolve(matrix))

 
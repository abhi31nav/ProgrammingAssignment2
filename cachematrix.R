

## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## This function returns the inverse of matrix. Initially it checks if the inverse 
## has been already computed or not. If computed, it skips computation.If not computed
## inverse is computed, sets the value in the cache with setinverse function. 

cacheSolve <- function(x, ...) {
 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}

# Output
#x = rbind(c(1, 2), c(3, 4))
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    3    4
#> cacheSolve(m)
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#> cacheSolve(m)
#getting cached data
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5

## these are used to create a special object and store a matrix
## and cache its inverse


## The function makeCacheMatrix contains the function for the following
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inver) inverse <- inver
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## check whether the inverse has already been calculated
## if so, get the inverse from the cache and skip the computation
## otherwise, calculate the inverse of the matrix
## set the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...){
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

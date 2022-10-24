#set and get the value of a matrix, and then set and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y){
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#Check to see if matrix inverse has already been calculated and cached. If not, calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)){
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- inverse(data, ...)
  x$setinverse(invrs)
  invrs
}


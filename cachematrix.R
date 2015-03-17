## makeCacheMatrix creates a Matrix with caches the invers
## cacheSolve then returns the invers (if exist returns the cache):
##  B <- makeCacheMatrix(A)
##  C <- cacheSolve (B)

## This function makeCacheMatrix creates a special "matrix"
## with is a list containing functions to 
## set a matrix | get a matrix | set the inverse | get the inverse

makeCacheMatrix <- function(m = matrix()) {
  
  invers <- NULL
  set <- function(y) {
    m <<- y
    invers <<- NULL
  }
  get <- function() m
  setinvers <- function(i) invers <<- i
  getinvers <- function() invers
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
  
}


## this function cacheSolve returns the inversed matrix of x
## if it is cached allready it returns the cached value 
## otherwise it calculates the inverse

cacheSolve <- function(x,...) {
  m <- x$getinvers()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("calculate data")
  data <- x$get()
  m <- solve(data)
  x$setinvers(m)
  m  
}

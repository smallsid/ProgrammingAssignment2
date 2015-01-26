## makeCacheMatrix function takes a matrix, computes its inverse and caches it 
## if we need to re-use the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## making sure that the '<<-' doesnt look for 'm' in global env
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x ## Returns the matrix passed to makeCacheMatrix
  setinv <- function(solve) m <<- solve ## Computes the inverse of the matrix
  getinv <- function() m ## Returns the cached inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve function checks for the matrix passed to it has a cached inverse
## else it caches the new matrix's inverse using the functions defined in 
## makeCacheMatrix. And returns the inverse to the user

cacheSolve <- function(x, ...) {
  m <- x$getinv() ## Checking if "x"s inverse is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## Reurn the cached inverse passed by getinv()
  }
  data <- x$get()
  m <- solve(data, ...) ##Computing the inverse of x
  x$setinv(m) ## Caching inverse of x
  m ## Returning the inverse to the user
}

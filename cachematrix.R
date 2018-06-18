## Two functions that are design to computate the inverse of a matrix and store it in a cache.
## If the matrix is later again asked, it will retrieve it from the cache without computing the inverse again


## function that return a vector of functions that store, get, compute inverse and get inverse of given matrix
makeCacheMatrix <- function(x = matrix()) {
  #set the inverse to NULL initially
  m <- NULL
  
  #cache the oroginal matrix and set inverse to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #retrive original matrix
  get <- function() x
  
  #compute the inverse matrix
  setinverse <- function(solve) m <<- solve
  
  #retrieve the inverse matrix
  getinverse <- function() m
  
  #return the special "vector" ith functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## function that computes inverse matrix, unless it is already computed, then it retrieves is from cache
cacheSolve <- function(x, ...) {
  m <- x$getinvese()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
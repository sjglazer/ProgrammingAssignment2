 
## this function is used to create a special matrix then can be used to 
## store, retrieve and calculate the inverse of an invertible matrix 
  
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL 
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
  } 
  
   
  get <- function() x 
  setinverse <- function(inverse) m <<- inverse 
  getinverse <- function() m 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) 
  
} 
  
  
## this function calculates the inverse of a matrix and stores
## the result in cache for quick retrieval later. It assumes that
## the matrix is invertible
  
cacheSolve <- function(x, ...) { 
  
  ## Return a matrix that is the inverse of 'x' 
  m <- x$getinverse() 
  if(!is.null(m)) { 
    message("getting cached data") 
    return(m) 
  } 
  data <- x$get() 
  m <- solve(data, ...) 
  x$setinverse(m) 
  m 
} 

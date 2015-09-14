## Following the cache of mean for a vector example, working on a similar assignment for 
## Caching Inverse of a matrix.. Calculating inverse of a matrix is computationally expense, hence
## saving it in memory and if the object is found use it, else compute new

## The first function makeCacheMatrix will do the following
## a. set the value of matrix
## b. get the value of matrix
## c. set the value of Inverse of matrix
## d. get the value of Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  ## initiatlize the inv object, which is going to store the inverse of matrix. 
  inv <- NULL
  
  ## Pt a. Setting the value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Pt b. Getting the value of matrix
  get <- function() x
  ## Pt c. Setting the value of Inverse of matrix
  setinverse <- function(inverse) inv <<- inverse
  ## Pt d. Getting the value of Inverse of matrix
  getinverse <- function() inv
  
  ## Return the matrix and the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve returns the Inverse of a matrix, if computed already then gets the result
## and skips the computation. If not then it computes the inverse, returns the result and 
## importantly stores it in cache.
## Assumption: Matrix is always invertible. No error handling code is included if that's not the case

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ## check whether found in memory
  if(!is.null(inv)) {
    message("From cache, skipping computation.")
    return(inv)
  }
  
  ## nothing in memory found, compute
  data <- x$get()
  inv <- solve(data,...)
  ## set the inv object
  x$setinverse(inv)
  ## return
  inv
}


## Testing
##x <- matrix(rnorm(4, mean=10, sd=1), nrow = 2)
##invx <- makeCacheMatrix(x)
##invx$get() ## returns a value
##invx$getinverse() ## returns NULL when called the first time.

## now call the cache solve function, so that it can return the inverse
## will compute first, and on repeated calls should just get it from memory
##cacheSolve(invx) ## computing it
##cacheSolve(invx) ## on call, see the message "From Cache, skipping computation" .. All good.


## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a special type of matrix 
## which stores its inverse also. 

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, 
       get = get, 
       setinv = setinv,
       getinv = getinv)
    
}


## cacheSolve takes a makeCacheMatrix and 
## returns the stored inverse (if already stored, else computes and stores the inverse)  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}

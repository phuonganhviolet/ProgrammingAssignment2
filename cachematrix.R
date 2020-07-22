## Put comments here that give an overall description of what your
## functions create a special matrix object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #### define the argument with default mode of "matrix"
  inv <- NULL ## initialize inv as NULL; will hold value of matrix inverse
  set <- function(y){  ## define the set function to assign new
    x<<-y  ## value of matrix in parent environment
    inv <<- NULL  ## if there is a new matrix, reset inv to NULL
  }
  get <- function() {x} ## define the get fucntion - returns value of the matrix argument
  setinverse <- function(inverse)(inv <<-inverse) ## assigns value of inv in parent environment
  getinverse <- function() {inv}  ## gets the value of inv where called
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
# This function computes the inverse of the special matrix returned by makeCacheMatrix above.
# If the inverse has already been caculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv<- solve(mat,...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}


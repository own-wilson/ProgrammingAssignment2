## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## creates or populates a special matrix depending 
## on passed parameters, and defines them in the 
## global environment
## defines functions to get and set 
## variables for the matrix passed to it
## Returns a list of the defined variables

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Takes an invertable matrix
## If the inversion of the matrix 
## has already been defined, the function 
## returns the inversion
## Otherwise, the function calculates the inversion,
## and returns that inverted matrix.

cacheSolve <- function(x, ...) 
{
  
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    return(inv)
  }
  myData <- x$get()
  inv <- solve(myData, ...)
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}

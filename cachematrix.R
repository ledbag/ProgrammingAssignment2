## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function is able to create a new object, which includes the Inverse Matrix and 
## the flag (Invers) to notify if the Inverse matrix exist or not.

## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
  {
  invers <- NULL

##Using the <<- operator we can store in variable x in  external environment
## and not in function environment.  
  
  set <- function(y) 
      {
      x <<- y
      invers <<- NULL
      }
  get <- function() 
    {
    x
    }

  setinvers <- function(solve) 
    {
    invers <<- solve
    }
  
  getinvers <- function() 
    {
    invers
    }
  
  list(set = set, get = get,setinvers = setinvers,getinvers = getinvers)
}


## This function calls the object created in special "matrix" returned by
## other function. If the inverse exists, the flag invers is different to Null
## So, we can get the inverse value from the "matrix" and dont calculate it.
## We are assuming that matrix is always invertible.

cacheSolve <- function(x, ...)
  {
  invers <- x$getinvers()
  if(!is.null(invers)) 
    {
    message("getting cached data")
    return(invers)
    }
  data <- x$get()             ##here I take the x value because the Invers didnt exist.
  invers <- solve(data, ...)
  x$setinvers(invers)         ##here I store the Invers for the future.
  invers
  }

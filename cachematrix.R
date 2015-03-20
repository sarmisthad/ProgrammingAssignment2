## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix  takes a matrix as an input and creates a 
## special "matrix" with the input. 
## It caches the input matrix, its inverse ,
## and a caching flag indicating if inverse is cached or not.

makeCacheMatrix <- function(x = matrix()) {

  # Cached data of the object, initialized with default values
  y <- NULL # variable to store a matrix
  m <- NULL # variable to store inverse of the given matrix
  z <- -1; # variable to store inverse caching flag


  # Functions to store and return data of the cached object
  setmatrix <- function(y) { #set the value of the matrix 
    x <<- y 
    m <<- NULL 
    z <<- 0; # inverse caching flag set to 0 for a new matrix
  } 
  getmatrix <- function()x # return the stored matrix
  setinverse <- function(inv) m <<- inv # set the inverse matrix
  getinverse <- function() m # return the inverse matrix
  setflag <- function(flg) z <<- flg # set the change flag
  getflag <- function () z # return the change flag
  

  # list of all functions of the cached object
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse =   setinverse, getinverse = getinverse, setflag = setflag, getflag = getflag) 

}


## Write a short comment describing this function
##cacheSolve function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse ##has already been calculated (and the matrix has not changed
##then the cachesolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # get the inverse of the object
  m <- x$getinverse()

  # check if the inverse is already cached and the matrix
  # has not changed after last caching, to return cached inverse
  if(!is.null(m)){ 
    if(x$getflag() == 1){
	 message("getting cached data")  
	 return(m) 
    }
    #message(" inverse present but matrix has changed")
  }

  # calculate inverse, cache it, and set inverse caching flag
  # to 1 to indicate inverse caching is done.
  data <- x$getmatrix() 
  f <- 1
  m <- solve(data, ...) 
  x$setinverse(m) 
  x$setflag(f)
  m 

}

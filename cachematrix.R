## makeCacheMatrix and cacheSolve 

## makeCacheMatrix creates a matrix and saves it in the cache for future use.
## Also creates a list of 4 sub-functions that can be called to perform operations on cached matrix.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL             ## sets variable inv to the NULL value, overwriting any previous value. 
  set <- function(y)      ## defines sub-function "set" 
  {
    x <<- y               ## takes local variable "y" passed to sub-function "set" and stores it in variable "x" in containing environment ( function "makeCacheMatrix()" )
    inv <<- NULL          ## sets variable "inv" in containing environment "makeCacheMatrix()" back to NULL 
  }
  get <- function() x     ## defines sub-function "get" to return the value stored in global variable "x" 
  setinv <- function(inverse) inv <<- inverse  ## Defines sub-fuinction setinv() takes the computed inverse matrix as an argument and sets it to the global variable "inv"
  getinv <- function() inv      ## Defines sub-fuinction getinv() and retrieves data from global variable "inv"
  list(set = set, get = get, setinv = setinv, getinv = getinv)     ## creates a global list object containing references to the sub-functions
}


## cacheSolve checks to see if a matrix has been previously 
## solved for its inverse and stored in the cache. If so, it 
## retrieves the solved inverse and displays it on the screen.
## If there is no matrix in the cache, the function computes the 
## inverse, stores it in the cache, and prints to the screen. 

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()       ## retrieves global variable "inv"
  if(!is.null(inv))       ## checks to see if there is a value other than NULL in "inv". If anything other than NULL, returnes message and value of "inv"
  {
    message("Getting Cached Data") ## prints message "Getting Stored Data" on screen
    return(inv)                   ## returns the value of "inv"
  }
  data <- x$get()               ## sets local variable "data" to value stored in global variable "x"
  inv <- solve(data,...)        ## computes inverse of variable "data" and stores in global variable "inv"
  x$setinv(inv)                 ## calls sub-function setinv and passes value stored in "inv" to function
  inv                           ## prints value of "inv"
}

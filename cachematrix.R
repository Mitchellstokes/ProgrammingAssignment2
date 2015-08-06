## We will store the matrix in variable x
## We will store the inverse of the matrix in variable xInverse
## Comments have been scattered throughout the function


## This function will create a matrix 'x' before creating an inverse matrix variable 'xInverse'.
## The function then sets inversion functions and applies them to the matrix variable

makeCacheMatrix <- function(x=matrix()) {
  
  xInverse <- NULL ##Initialise the inverse matrix variable
  
  set <- function(y) {
    
    x <<- y
    xInverse <<- NULL ##Initialise the inverse matrix variable in this environment
    
  }
  
  get <- function() x ## Store/Return the matrix to the get variable
  setInverse <- function(inv) xInverse <<- inv ## Create the inversion
  getInverse <- function() xInverse ## Store/Return the inverse matrix to the getInverse variable
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) ## Store the variables the matrix to be called later
  
}


## This function will attempt to find the variable xInverse in the previous function
## If this variable does exist, the function will return the Inverse Matrix variable 'xInverse'
## If this variable does not exist, that means the 'makeCacheMatrix' function has not been run
## In this case, the function will call the previously defined functions within the 
## 'makeCacheMatrix' function and solve the matrix itself, before returning it

cacheSolve <- function(x,...) {
  
  xInverse <- x$getInverse() ## Attempt to retrieve the inverse matrix and store it in variable XInverse
  if(!is.null(xInverse)) { ## Check if xInverse is not null, if so exit and return xInverse, if not continue
    message("Retrieving the Inverse Matrix")
    return(xInverse)
  }
  getAgain<-x$get() ## Get the matrix again
  xInverse<-solve(getAgain,...) ## Solve the matrix and store to variable xInverse
  x$setInverse(xInverse) ## Get inverse of xInverse using function defined above
  xInverse ## Return the inverse matrix
}

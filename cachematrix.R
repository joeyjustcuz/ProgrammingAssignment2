## these functions return the inverse of matrix... it first checks to see if a cache value is stored.
## otherwise it calculates and returns the cache inverse

## these are the basic get and set functions for the cache. 
## it first initializes the inverse (called inv) of matrix x to NULL
## then it creates helper functions set/get/setinv/getinv to handle assigning and setting the cache

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse to NULL
  inv <- NULL
  
  #again initialze the inverse to NULL and set the matrix to y
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #return the matrix
  get <- function() x

  #set the cache inverse value
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  #return the cache inverse value
  getinv <- function() inv
  
  #not sure why we need this but it was in the example. sue me
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv
       )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the value of the inverse using the getinv function
  inv <- x$getinv()
  
  # check to see if the value assigned to inv is NULL. 
  # if it is not the inverse is assigned as return the cache value
  if (!is.null(inv)){
    message("inverse is calculated, returning cache data")
    return(inv)
  }
  # inverse is not calculated, do the manual work
  else{
    #get the matrix
    data <- x$get()
    
    #assign the inverse using solve function
    inv <- solve(data, ...)
    
    #set cache value as inv
    x$setinv(inv)
    
    return(inv)
  }
  
}

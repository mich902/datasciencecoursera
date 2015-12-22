## Put comments here that give an overall description of what your
## functions do

# Caching the Inverse of a Matrix where makeCacheMatrix will create a matrix object that can cache its inverse and cacheSolve will compute the inverse of a matrix returned by makeCacheMatrix.  

## Write a short comment describing this function - A specical matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {

#Setting initial value of matrix inverse
    
    inv_cache <- NULL
    
    #Function to get the value of the matrix
    
    get <- function() x
    
    # Setting functions to get and update the matrix inverse, setInv() uses <<- to update value in parent.frame (not part of global enviroment)
    
    setInv <- function(inv_toCache) inv_cache <<- inv_toCache
    getInv <- function() inv_cache
    
    # Return a list containing the object functions to use later
    
    list(get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function: This will compute the inverse of a matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        cacheSolve <- function(x){
  
  # Use the getInv() function to retrive the current value in inv_cache
  inv_cache <- x$getInv()
  
  # If the cached inverse exhist then return it.
  if(!is.null(inv_cache)) {
    
    message("getting cached data")
    
    return(inv_cache)
    
  }
  
  # Use the get() to retrieve the matrix 
  data_matrix <- x$get()
  
  # Calculating inverse of the matrix 
  inv_toCache <- solve(data_matrix)
  
  # Use the setInv() to update the inverse value in the cache
  x$setInv(inv_toCache)  
  
}

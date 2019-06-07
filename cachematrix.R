## Overall a special matrix is introduced that can cache its inverse. 
##This is done by creating a matrix 'x' using the function makeCacheMatrix. 
##Generating the cached inverse matrix 'd' through the function cacheSolve.



# makeCacheMatrix uses the given data to create a 'special' squared matrix through 4 specified functions 
makeCacheMatrix <- function(x = matrix()) {
        d <- NULL    
  set <- function(y) { #loops through the variable looking for 'x' while clearing the cache
    x <<- y
    d <<- NULL 
  }
  get <-function() x #Define the matrix with the function 
  setinverse <- function(inverse)d <<- inverse #Runs the inverse 
  getinverse <- function()d #Variable defined as the inverse matrix
        
  #generates a list with the 4 functions 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


#cacheSolve takes in the matrix created by the makeCacheMatrix and produces the inverse,
#if it already exists it can obtain the inverse from the cache 

cacheSolve <- function(x, ...) {
        d <-x$getinverse()  #how x matches the inverse generating a message if the cache exists
    message("getting cached data") 
    return(d)
  }
  data <- x$get() #Run the get function from ... and store in data (getting the value of the matrix)
  d <- solve(data,...)  #run the %*% in the arguments (data,...) store as 'd' (inverse matrix) 
  x$setinverse(d) #Add the results to the cache
  d 
        ## Return a matrix that is the inverse of 'x'
}

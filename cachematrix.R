## Following two functions can be used to chache the information on inverse of a 
## cached matrix.


## 'makeCacheMatrix' will create a cache which is a list of functions that will 
## represent the data on cached matrix and its inverse, and the means of using 
## the cache by returning a list of functions.


makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  # Initializing a dummy variable to store the inverse of the function.
  
  set <- function(y = matrix()){
    x <<-y
    inv<<-NULL
  }
  # 'set' along with 'get', 'setinv', and 'getinv' will be the functions returned 
  # as a list returned by makeCacheMatrix to set the working matrix. 
  # It will assign the matrix y to x in the cache and then it will reset 
  # inv to NULL to be assigned by 'setinv'.
  
  get <- function() x
  # 'get' will be another function in the list returned by makeCacheMatrix that 
  # will return the cached matrix.
  
  setinv <- function(inverse) inv<<-inverse
  # 'setinv' will store the inverse  of the matrix returned by 'get' in the cache.
  
  getinv <- function() inv
  # 'getinv' will return the inverse stored in the cache.
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  # 'makeCacheMatrix' is now returning the data stored in the cache for the 
  # inverse and the means to manipulate that data as a list of function. 

}


## 'cacheSolve' will manipulate the cache 'x' by either checking if the inverse 
## is already stored and if it isn't then it will store the inverse of the matrix
## in the cache. 

cacheSolve <- function(x, ...) {
  # Returns the inverse of the matrix stored in the cache x. If the 
  # the inverse is not in the cache it finds the inverse and store it in
  # cache for later use before returning it.
  
  inv<-x$getinv()
  # Store the inverse of the matrix from the cache into the dummy variable 
  # inv'
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # Check if the inverse stored in the cache is not null and then return the 
  # inverse found in the cache.
  
  data<-x$get()
  # Store the matrix from the cache into a dummy variable data.
  
  inv<-solve(data,...)
  # Calculate the inverse of the matrix in the cache and storing it in inv. 
  
  x$setinv(inv)
  # Store the inverse in the variable 'inv' to the cache.  
  
  inv
  # Returning the inverse of the matrix in the cache 'x'

}

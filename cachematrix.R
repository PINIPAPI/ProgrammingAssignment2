## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The first function, makeCacheMatrix creates a special "Matrix", which 
##is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inv
##get the value of the inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                               #Set to null the inverse
  set <- function(y) {                      #Fuvtion to set yhe matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                       #Function to return the matrix
  setinv <- function(inver) inv <<- inver   #Fuction to assign the inverse
  getinv <- function() inv                  #Function to return the inverse
  list(set = set, get = get,                #List of the functions of the
       setinv = setinv,                     #main function
       getinv = getinv)
}


## Write a short comment describing this function

##The following function calculates the inverse of the special "matrix" created 
##with the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the data and sets the 
##value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()                   #Assigns the cached value to inv
  if(!is.null(inv)) {                 #Validate if take the value cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                     #Takes the value from the inseted matrix
  inv <- solve(data, ...)             #Makes the operation
  x$setinv(inv)                       #Assigns the value to the makeCacheMatrix list function
  inv                                 #Shows the result
}

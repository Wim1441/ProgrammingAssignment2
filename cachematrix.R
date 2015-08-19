## Put comments here that give an overall description of what your
## functions do
# The first function creates a special "matrix" object that can cache 
# its inverse. The second function computes the inverse of the special "matrix"
# returned by the first function. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from cache.

## Write a short comment describing this function
# The function makeCacheMatrix creates a special "matrix", which is really a 
# list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      #empty inverse when function called
      i <- NULL
      
      #set value
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
      # get value
      get <- function() x
      
      #set inverse
      setsolve <- function(solve) i <<- solve
      
      #get inverse
      getsolve <- function() i
      
      #create special matrix
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Write a short comment describing this function
# The function cacheSolve first checks if the mean has already been calculated.
# If so, it gets the mean from the cache and skips the computatio. Otherwise, it
# calculates the mean of the data and sets the value of the mean in the cache
# via the setmean function

cacheSolve <- function(x, ...) {
      # check if already solved
      i <- x$getsolve()
      
      # if already solved return inverse and close function
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      # else get data
      data <- x$get()
      
      # calculate inverse
      i <- solve(data, ...)
      
      # set inverse as solution
      x$setsolve(i)
      
      # return inverse
      i
}

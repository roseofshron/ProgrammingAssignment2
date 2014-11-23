## The ultimate goal is to be able to take in a matrix, and solve and cache its inverse, 
## and then produce the inverse, either by detected the cached inverse, or solving it for the first time

## this first function sets up the objects that we will need to later be able
## to detect if there is a cached inverse

makeCacheMatrix <- function(x = matrix()) {      # creates the first function, and tells it to expect matrices
      i <- NULL                                  # starts by setting what will be the cache inverse to NULL
      set <- function(y) {                       # creates another function that allows us to change the first matrix, if we want to
            x <<- y                              # changes the first input to the new one
            i <<- NULL                           # resets cached inverse back NULL, if applicable
      }
      get <- function() x                        # makes 'get' produce the original matrix
      setinverse <- function(solve) i <<- solve  # makes i the inverse
      getinverse <- function() i                 # makes 'getinverse' produce i
      list(set = set, get = get,                 # creates a list of the objects we've just created
           setinverse = setinverse,
           getinverse = getinverse)
}


## This second function either detects a cached inverse, and produces it, 
## or solves for the inverse, produces it and stores it

cacheSolve <- function(x, ...) {                 # creates a second function which will get the cached inverse
      i <- x$getinverse()                        # gets the value of the inverse of x
      if(!is.null(i)) {                          # determines if i isn't NULL...
            message("getting cached data")       # ...if so, returns message 'getting cached data'
            return(i)                            # and produces cached inverse
      }
      data <- x$get()                            # if i is NULL, creates 'data', which is the value of x
      i <- solve(data, ...)                      # solves for the inverse
      x$setinverse(i)                            # stores that inverse...
      i                                          # ...and then produces it
}

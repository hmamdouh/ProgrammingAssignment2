makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse value
  i <- NULL
  #the following function caches the value of matrix for the first time

  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #Gets the value of x to be used for inverse for the first time, if not the 
  #first time, then the inverse had been calculated already before 
  get <- function() x
  #after first time of calculating inverse, it will be pushed to cache here
  setinv <- function(inv) i <<- inv
  #will be used if this is the first time and inverse had been calculated before
  #checkSolve function
  getinv <- function() i
  #The return list from makeCahceMatrix is a list of
  #1) set function which will push the x matrix to cache for first time
  #2) get the value of x matrix, if the inverse wasn't calculated before to get
  #inverse for the amtrix in the cacheSolve function
  #3)setinv function will be passed the value of inverse from the 
  #cacheSolve function
  #At the beginning of the cacheSolve function it checks using getinv to see
  #if the inverse function has been calculated
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #let's see if the inverse has been cached before
  i <- x$getinv()
  if(!is.null(i)) {
    #inverse has been cached before and we will quickly retrieve it
    message("getting cached data")
    #return the inverse value and exit the function
    return(i)
  }
  #as we came here, then that means the inverse hadn't been cached before
  data <- x$get()
  #Calculate the inverse of the data for the first time
  i <- solve(data, ...)
  #pass it to the setinv function handle in the x object to be cached
  x$setinv(i)
  #return the inverse value for the first time now
  i
}
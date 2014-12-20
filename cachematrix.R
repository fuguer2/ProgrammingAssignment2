########################################################
## makecacheMatrix
########################################################
## creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
########################################################
makecacheMatrix <- function(x = numeric()) {
  
  # initialize the variable that will hold cached solutions
  m <- NULL
  
  # declare the function to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # declare the function to get the matrix
  get <- function() return(x)
  
  # declare the function to compute and cache the inverse
  setSolve <- function(solve) m <<- solve
  
  # declare the function to retrieve the cached inverse
  getSolve <- function() return(m)
  
  # return the list of functions
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}

########################################################
## cacheSolve
########################################################
## Calculates the inverse of the special "vector" created with makecacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setSolve function.
########################################################
cacheSolve <- function(x, ...) {
  
  # check for cached solution
  m <- x$getSolve()
  
  # if cached solution is found, return result
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # otherwise retrieve the value and solve the matrix
  data <- x$get()
  m <- solve(data, ...)
  
  # cache the solution
  x$setSolve(m)
  
  # return the solution
  return(m)
}

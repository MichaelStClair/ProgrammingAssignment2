## Homework assignment for week 3 of R Programming (Coursera)
##   (the name of the assignment is ProgrammingAssignment2)

## Create a matrix object with four methods
## it can set itself
## it can get itself
## it can solve itself aka set its own solution
## it can get the solution of itself
##
## this uses the standard solve() function so the matrix must be square!
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Create a function to get solution of above matrix object
## If already solved, return a cached version of the solution
## by ONLY using the setsolve method when the getsolve method returns null
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)
}


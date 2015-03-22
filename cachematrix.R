## Put comments here that give an overall description of what your
## functions do
## Week 3 project - JLN
## Write a short comment describing this function
## MakeCasheMatrix creas a "matrix" object that cache its inverse
## = = = =
## based code on the example provided MakeVector
## Original MakeVector code
## makeVector <- function(x = numeric()) {
##  m <- NULL
##  set <- function(y) {
##    x <<- y
##    m <<- NULL
##  }
##  get <- function() x
##  setmean <- function(mean) m <<- mean  [change from mean to inverse]
##  getmean <- function() m
##  list(set = set, get = get,       [display results]
##       setmean = setmean,
##       getmean = getmean)
##}
## Start
## makeCacheMatrix - "matrix" object

makeCacheMatrix <- function(x = matrix()) {
## clear mkinv 
  m <- NULL 
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
# get inverse
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
## inverse of the original input in above makeCacheMatrix
## calcuates then search previous result to see if result 2 is a duplicate of previous results
## Original code provide by instructor for cachemean
## cachemean <- function(x, ...) {
##  m <- x$getmean()
##  if(!is.null(m)) {         ["matrix has not chang, data is there]
##    message("getting cached data")
##    return(m)
##  }
##  data <- x$get()
##  m <- mean(data, ...) [use solve instead of mean per instruction]
##   x$setmean(m)
##   m
##}
## start
## cacheSolve - inverse of Matrix code

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    m <- x$getinverse()
## determine if inverse is already calcuated
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
## get the invert (of mean) using solve() 
   mkinv <- solve(data, ...)
    x$setinverse(m)
    m
  }
}

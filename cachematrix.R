## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a 'matrix' function, x, which is a list containing a
## function that will set the matrix, get the matrix, set the inverse matrix, 
## and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## this function calculates the inverse matrix. If m is NOT NULL, in other words,
## if there is a matrix stored in m, then it will 'get cached data' and return 
## m.  If not, it will calculate the inverse of the data and set the matrix 
## in the cache via setmatrix.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
  ## Return a matrix that is the inverse of 'x'


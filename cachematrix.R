## Jay Urbain, 9/15/2014
## jay.urbain@gmail.com
##
## cachematrix.R
# R function that is able to cache time-consuming matrix inverse computations. 
# 1) Given a matrix, A, that has an inverse, construct a cache Matrix:
# AA<-makeCacheMatrix(A).
# 2) Give a cache Matrix, AA, compute the matrix inverse iff the inverse
# has not already been computed and the values have not changed!

## Test case showing usage:
"
> source('cachematrix.R')
> A<- matrix( c(4,3,3,2), nrow=2, ncol=2, byrow = TRUE)
> A
[,1] [,2]
[1,]    4    3
[2,]    3    2
> AA<-makeCacheMatrix(A)
cacheSolve(AA)
[,1] [,2]
[1,]   -2    3
[2,]    3   -4
"

#########################################################################
## makeCacheMatrix
## Creates a special "matrix" object that can cache its inverse.
## Really a list containing a function to:
##   set the value of the vector
##   get the value of the vector
##   set the value of the mean
##   get the value of the mean

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

#########################################################################
## cacheSolve
## Computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has 
## not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

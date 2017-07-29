# makeCacheMatrix and cacheSolve work in tandem and allow caching of a matrix inverse, 
# to save computation time.

#---------------------------------------------------------------------------------------#
#makeCacheMatrix constructs a list of 4 elements, where each element is a function
#operating on matrix x. the functions are: setm, getm, setinv, getinv

#setm, stores a matrix in x (this option is added beyond the scope of the assignment)
#getm, returns matrix x to caller
#setinv - stores the inverse of matrix x in invx
#getinv - returns the inverse of the matrix (from invx)
#---------------------------------------------------------------------------------------#
makeCacheMatrix <- function(x = matrix()) {

     #invx stores the inverse of x Initialize to NULL. 
     #This assures correct behavior when makeCacheMatix is called directly
     #with a matrix as an argument.
     invx <- NULL
     
     list(
          #set matrix x (can be used to override initial matrix)
          setm = function(y=matrix()) {
               x <<- y
               invx <<- NULL #when a new matrix is set, make sure invx is nulled!
          },
          
          #return matrix m
          getm = function() {
               x
          },
          
          #store the inverse of m in invx - effectively caching it
          setinv = function(inverse) {
               invx <<- inverse
          },
          #get the inverse of m
          getinv = function() {
               invx
          }
          
     )# end list
     
     
} # end makeCacheMatrix

#---------------------------------------------------------------------------------------#
#cacheSolve returns the inverse of matrix x (works in tandem with makeCacheMatrix). 
#first, attempts to get a cached copy of the inverse. if a cached inverse 
#doesn't exist, calculates it and stores in cache.
#---------------------------------------------------------------------------------------#

cacheSolve <- function(x, ...) {
     
     #attempt to get inverse from cache 
     invmat <- x$getinv()
     
     #return cached inverse, if exists
     if(!is.null(invmat)) {
          message("cached inverse found")
          return(invmat)
     }
     
     #if cached inverse doesn't exist, get matrix, calculate its inverse and cache it.
     message("no chached inverse found, calculating inverse")
     mat <- x$getm()
     #NOTE: no local checking that that mtrx is invertible.
     invmat <- solve(mat, ...)
     #store the inverse into a cache.
     x$setinv(invmat)
     
     #return the inverse
     return(invmat)
     
     }

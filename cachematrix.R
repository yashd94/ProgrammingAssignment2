## These functions will calculate the inverse of a sqaure matrix (assuming that it is invertible), and cache the inverted matrix. This will ensure that if the matrix remains unchanged, the calculation to ##invert the matrix need not be re-done over and over. The calculation will only be done in case the matrix has changed.

## This function does the following: set the value of a matrix, get the value of a matrix, set the inverse of a matrix, and get the inverse of a matrix. It also caches the calculated inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  Set <- function(s)
  {
    y <<- s
    inv <<- NULL
  }
  Get <- function() y
  SetInverse <- function(solve) inv <<- solve
  GetInverse <- function() inv
  list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)

}


## This function takes as an argument the list returned by the makeCacheMatrix function, and checks to see if the inverse of the matrix provided has already been calculated. If yes, it fetches the inverse## that has been cached, and if no, computes the inverse of the matrix provided. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 inv <- x$GetInverse()
 if (!is.null(inv))
 {
   message("Fetching cached inverted matrix.")
   return(inv)
 }
 mtrx <- x$Get()
 inv <- solve(mtrx)
 x$SetInverse(inv)
 inv

}

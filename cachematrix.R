## makeCacheMatrix is function that has a list of functions that stores your input
## of singular matrix (in get & set functions) & stores the inverse(in getinverse & setinverse)  
## While cacheSolve solves and returns the inverse of the input matrix if not previously stored.

## makeCacheMatrix() takes an input of a matrix and initially sets the variable 'i' as null
## and uses the set() to define new inputs and reset the 'i' to null. This is later stored in get. 
## With '<<', we  store the new input as x and resetted 'i' in not just the set function, 
## but in the entire makeCacheMatrix(). The setinverse() sets the calculated value of the inverse
## of the matrix which has been calculated and is stored in getinverse(). 
## Finally, all 4 functions are put in a list.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## cacheSolve() first checks if the inverse of matrix has been stored in 'i'. If it is stored, 
## it returns the inverse after printing "getting cached data".If not(ie the value of i will be null)
## and it will find the inverse with the solve function and later set it in the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

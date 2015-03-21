## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a list of four functions and creates two internal variables x and m

## x is the original matrix
## m is the inverse of x
## when the cacheMatrix is created m should be null (no inverse yet)
## whenever x is set m should be null (because x is a new value so old m is invalid)

## this function can only get & set x or get & set m
## it doesn't calculate m

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      
      x <<- y
      m <<- NULL
      
    }
    
    get <- function() x
    
    setinv <- function(inv){
      m <<- inv
    } 
    
    getinv <- function() {
      m
    }
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function

## This function calculates m , inverse of hte matrix in the makeCacheMatrix list
## if m is already present than it just returns m
## if m is not available it calculates it then sets it using the setinv function available
## in makeCacheMatrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## x is not a matrix but a makeCacheMatrix list containing a matrix
  
      m <- x$getinv()
      
      if(!is.null(m)) {
        
        message("getting cached matrix inverse")
        return(m)
        
      }
      
## if we need to find m grab the matrix, solve it, then set m in cache\

      data <- x$get()
      m <- solve(data, ...)
      
      x$setinv(m)
      
      m
  
}

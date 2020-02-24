#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeChacheMatrix <- function(x = matrix()){
  
  invr <- NULL
  set <- function(y){
    
    x <<- y
    invr <<- NULL
    
  }
  
  get <- function() x
  
  set_inverse <- function(inverse) invr <<- inverse
  get_inverse <- function() invr
  
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse )
  
  
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

chachesolve <- function(x, ...){
  
  invr <- x$get_inverse()
  if(!is.null(invr)){
    
    message("getting chached data")
    return(invr)  
  }
  
  mat <- x$get()
  invr <- solve(mat, ...)
  x$set_inverse(invr)
  invr
  
  
}

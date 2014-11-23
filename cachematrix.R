#Matrix function that stores a matrix and the matrix inverse
#and a function that returns a cached object or 

#this is a matrix "object" with functions to get and set
#the matrix or matrix inverse

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  
  #matrix setter function
  set <- function(mat) {
    m <<- mat
    inv <<- NULL
  }
  
  #matrix getter function
  get <- function(){
    m
  }

  #inverse getter function
  getInverse <- function() { 
    inv
  }
  
  #inverse setter function
  setInverse <- function(i){
    inv <<- i
  }
  
  #return the functions defined
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


# calculates and returns the matrix inverse for "makeCacheMatrix"
# if the inverse does not already exist
# otherwise it returns the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("Using cached inverse")
    return(inv)
  }
  
  #if the function hasn't returned yet then we need to calculate the inverse
  message("calculating inverse")
  m <- x$get()
  
  x$setInverse(solve(m))
  
}

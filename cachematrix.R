## The function caculates the inverse of the matrix and store in the Cache. 
## If the matrix doesn't change, get the Inverse from Cache instead.


## This function take a matrix as argument and create a special vector 
## of functions that help set/get the value of the matrix, set/get the 
## Inverse matrix to/from the cache.

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL  # initialize the cache as NULL
  set<-function(y){
    x<<-y
    i<<-NULL      # when we set the new matrix value, the cache will be initialized as NULL
  }
  get<-function() x  # get value of the matrix 
  setInverse<-function(inverse){i<<-inverse} # put the inverse into Cache
  getInverse<-function() i  #return the cached 
  # return the vector of 4 functions
  list(set = set,get= get,getInverse = getInverse,setInverse = setInverse) 
  
}


## The Input of the function is the special vector from makeCaheMatrix() function
## The Outputof the function is the Inverse matrix from cached memory if exists. 
## Otherwise, it returns the newly calculated Inverse matrix


cacheSolve <- function(x, ...) {
  i = x$getInverse()  # get the the Inverse from the cache
  if(!is.null(i)){
    message("Retrieve the Inverse matrix from cache")
    return (i)  # retrieve tha cached Inverse matrix
  }else {  # there is no cached Inverse matrix
    m<-x$get()
    i= solve(m) # caculate the inverse
    x$setInverse(i) # put the Inverse to the Cache
    i               # return the Inverse 
  }      
  ## Return a matrix that is the inverse of 'x'
}

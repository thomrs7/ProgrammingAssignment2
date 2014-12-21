## Put comments here that give an overall description of what your
## functions do

## Uses solve() to calc the inverse of a matrix 

#Enter the function with a matrix
makeCacheMatrix <- function(x = matrix()) {
  #a place for the inversed version
  inversed_matrix <- NULL
  
  #Set the values
  set <- function(y) {
    x <<- y
    inversed_matrix <<- NULL
  }
  
  #Get matrixes value
  get <- function() x
  
  #Set the inversed matrix
  setinverse <- function(solve) inversed_matrix <<- solve
  
  #Get the matrix
  getinverse <- function() inversed_matrix
  
  #The final list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Given a cachable matrix return the inverse
# x:  makeCacheMatrix()

cacheSolve <- function(x, ...) {
  #Get the cached matrix
  inversed_matrix <- x$getinverse()
  
  #If we have something return it
  if(!is.null(inversed_matrix)) {
    message("getting cached data")
    return(inversed_matrix)
  }
  
  #Booo no cached data let get the matrix
  data <- x$get()
  #Invert it
  inversed_matrix <- solve(data, ...)
  #Cache the bad boy for later use
  x$setinverse(inversed_matrix)
  return(inversed_matrix)
}

# Example: 
#    > the_matrix <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
#    > cached_matrix <- makeCacheMatrix(the_matrix)
#    > cacheSolve(cached_matrix)
#        <inverse matrix here>
#    > cacheSolve(cached_matrix)
#        getting cached data
#       <inverse matrix here>
## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function





makeCacheMatrix <- function(OriginalInput = matrix()) {
  #This function takes the input of a numeric matrix and returns a list consists the following 3 functions.
  
  # Note: The makeCacheMatrix function almost excatly replicates the getmean function example given from the course website, 
  # but with only two differences: 
  # 1) the names of the variables are changed to better reflect it's value and purpose; and 
  # 2) the set function are excluded from the code, as it is not needed.
  
  #The followng Assign initial value to SavedInverse, which will later be used to store the value of the inverse matrix
  SavedInverse <- NULL
  
  #The following is the "set" function from the "getmean" example, which is excluded in this program.
  # set <- function(y) {
  #  x <<- y
  #  m <<- NULL
  #}
  
  # The get function extracts the numeric value of the original matrix (i.e. the input matrix)
  get <- function() OriginalInput
  
  #The setinverse function assigns the value of the CalculatedInverse matrix
  #to SavedInverse in the global environment, so that the value of the SaveInverse can be called
  #from functions other than makeCacheMatrix.  
  #Specfically the cacheSolve function can later call and use this SavedInverse value from cache
  setinverse <- function(CalculatedInverse) SavedInverse <<- CalculatedInverse
  
  #This function extracts the Saved value of the inverse matrix
  getinverse <- function() SavedInverse
  
  #The function makeCacheMatrix returns a list of 3 functions, 
  #and again the "set" fucntion from the "getmean" example.
  #namely get, setinverse, and get inverse. 
  #Note: the "set function" from the getmean example 
  #list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  list(get = get, setinverse = setinverse, getinverse = getinverse)
}









## Write a short comment describing this function

cacheSolve <- function(CreatedObject, ...) {
  # This function inputs the the object (i.e. a list of specific functions) created by the makeCacheMatrix function, 
  # and returns the inverse of the original input matrix that has been entered into makeCacheMatrix function.
  
  # If there is an existing "Saved Inverse matrix", the following extracts it and returns it. 
  InversedMatrix <- CreatedObject$getinverse()
  if(!is.null(InversedMatrix)) {
    message("getting cached data")
    return(InversedMatrix)
  }
  
  # If there isn't an existing Saved Inverse matrix, the following calculates the inverse matrix and then saves the inverse matrix.
  
  # First CreatedObject$get() would extract the original the value of input matrix to the variabe OriginalInputMatrix
  OriginalInputMatrix <- CreatedObject$get()
  
  # Then solve(OriginalInputMatrix) calcualtes the inverse and assign the inverse to InversedMatrix
  InversedMatrix <- solve(OriginalInputMatrix,...)
  
  # CreatedObject$setinverse store the value of m in cache (extractable from global environment) for future extraction
  
  CreatedObject$setinverse(InversedMatrix)
  InversedMatrix
}

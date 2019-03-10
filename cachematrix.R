## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Ninad inserted comment
## This function takes creates a list of 4 functions that can cache the inverse of a matrix
## 1. Sets the value of a matrix to be inversed
## 2. Gets the value of a matrix to be inversed
## 3. Sets the inverse of a matrix to a cached variable
## 4. Gets the inverse of a matrix that has been cached(NULL if not cached)

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  
  ## Set the input
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  
  ## Get the input
  get<-function()x
  
  ## Set the inverse value so that its cached
  setInv<-function(inv)inverse<<-inv
  
  ## Get the inverse value that was cached
  getInv<-function()inverse
  
  ##returns a list of the functions
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function

## This function solves for the value of the inverse from the matrix that was sent as an argument to the "makeCacheMatrix" function
## If cached value is not available it will calculate the inverse and solve it and store it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ##Get the value of inverse (NULL if not cached)
  inverse<-x$getInv()
  
  ## Check if the inverse is already cached
  if(!is.null(inverse)){
    print("Getting Inverse from Cached data")
    
    ##Get cached inverse
    return(inverse)
  }

  ## Start solving for getting the inverse  
  data<-x$get()
  
  ## This condition takes the approx determinant of the matrix if its value is close to 0 the inverse will not be computed
  if(round(det(data))!=0){
    inverse<-solve(data)
  }
  
  ## Cache the inverse
  x$setInv(inverse)
  
  ## Return the inverse value
  inverse
}

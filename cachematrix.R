## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix 'x' and stores its inverse and it locally

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set <- function(y){
    x<<-y
    s<<-NULL
  }
  get<- function()x
  setinv<-function(solve) s<<- solve
  getinv<-function() m
  list(set=set,get = get, setinv = setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) { #checks to see if the data has been cached and returns the solved matrix if it has been solved
    message("getting cached data") #prints a message to the console
    return(s)
  }
  data <- x$get() #gets the data from storage
  s <- solve(data, ...) #takes the inverse of the matrix
  x$setinv(s) #stores the inverse in the cache
  s #returns the inverse

}

## These functions are used in conjunction to take the inverse of a matrix.  To save time they check if the inverse
## has already been stored.  Assume that all matricies passed are invertible.

## This function takes a matrix 'x' and outputs a list object.  It initially sets the inverse to NULL

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL              # s is the inverse variable and it's set to NULL everytime makeCacheMatrix is called
  set <- function(y){  # this function creates a new matrix and resets the inverse 's' to null 
    x<<-y
    s<<-NULL
  }
  get<- function() { x }           #returns value of the original matrix
  setinv<-function(inv) {s<<- inv} #stores the new value of the inverse using superassignment
  getinv<-function() {s}           #returns the cached value
  list(set=set, 
       get = get, 
       setinv = setinv, 
       getinv=getinv)              #creates the output object of type list with 4 variables of the 4 functions
}


## This function checks to see if the matrix has been solved previously and if not takes the inverse of the matrix
## and stores it

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) { #checks to see if the data has been cached and returns the inverse matrix if it has been solved
    message("getting cached data") #prints a message to the console
    return(s)
  }
  data <- x$get()       #assigns the data from the stored value only if there was no inverse already stored
  s <- solve(data, ...) #takes the inverse of the matrix
  x$setinv(s)           #stores the inverse in the cache
  s                     #returns the inverse

}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix returns a list that contains a function to:
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(inv) inver <<- inv
  getinver <- function() inver
  list(set= set,get=get,setinver = setinver,getinver=getinver)
}


## Tries to fetch inverse from cache and returns it.
## If no value is retrieved then calculates inverse using solve.

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data,...)
  x$setinver(inver)
  inver
}

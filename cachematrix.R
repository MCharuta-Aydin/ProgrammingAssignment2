## Two functions are written to cache the inverse of matrix
## One creates an object that can cache the inverse
## the other checks if cache exists if yes it uses it if not calculates and updates 

## the below function creates a special "matrix" object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
                   x<<-y
                   m<<-NULL
                   }
  get<-function()x
  setsolve<-function(solve) m<<- solve
  getsolve<- function()m
  
  list(set=set, get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}


## Function that checks if the inverse of matrics was calculated and exists in the cache
## if yes "if(!is.null(m))" it returns the cache m and exits function
## if no it calculates the inverse and saves as cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

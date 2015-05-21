## These functions allow you to cache the inverse of a matrix.
## Assign the results of makeCacheMatrix to a variable; call cacheSolve on the variable
## to get its cached inverse

## makeCacheMatrix creates a wrapper of getters/setters for a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  m<-NULL
  
  #Initialise the matrix
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  
  #Returns the initial matrix
  get<-function() x
  
  #Internal method - do not call directly!
  #Used by cacheSolve to calculate the inverse of the matrix
  setSolve<-function(solve) m<<-solve
  
  #Internal function - do not call directly!
  #Used by cacheSolve to check if there's already a cached result 
  getSolve<-function() m  
  
  #declare the methods available 
  list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}

## call cacheSolve on the wrapper to obtain the matrix's inverse

cacheSolve <- function(x, ...) 
{
  #Check if there's already a result
  m<-x$getSolve()
  
  #if m has a value, then there's already a cached result.
  #print the cached result and exit the function, returning m
  if(!is.null(m))
  {
    message("Getting cached data")
    return(m)
  }
  
  #get the stored matrix
  data<-x$get()
  
  #calculate the inverse of the matrix
  m<-solve(data)
  
  #cache the inverse in the wrapper
  x$setSolve(m)
  
  #return the inverse
  m
}

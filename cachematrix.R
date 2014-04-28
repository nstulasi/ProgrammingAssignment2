## Put comments here that give an overall description of what your
## functions do

## Function creates a list of functions that set a matrix variable, get a matrix,
# set the inverse and get the inverse. Input: Matrix, Output: List of functions


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set=function(y) {
    x<<-y
    inv<<-NULL
  } 
  get=function() x
  
  
  setinv=function(x){
    inv<<-solve(x)}
  
  getinv=function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Function gets the cached inverse if inverse has already been calculated for a given matrix. 
# If not, the inverse is calculated and cached.Input:List of functions, Output:Matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse value")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  return(inv)
}

## Put comments here that give an overall description of what your
## functions do

## this function will create a special matrix that can calcualte its own inverse
##it uses lexical scoping to pass the values between different environment

makeCacheMatrix <- function(x = matrix()) {

    mINv<-NULL
    set<-function(y){
      x<<-y
      mINv<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) mINv<<- solve
    getmatrix<-function() mINv
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
  }


## below function checks if the inverse of given matrix is stored and returns the same
## if inverse is not found in cache then it calculates the same


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mInv<-x$getmatrix()
    if(!is.null(mInv)){
      message("getting cached inverse matrix")
      return(mInv)
    }
    matrix<-x$get()
    mInv<-solve(matrix, ...)
    x$setmatrix(mInv)
    mInv
  
}

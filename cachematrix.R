# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to, set the value of the matrix
# get the value of the matrix and set the value of the inverse and 
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# The following function calculates the inverse of the matrix created
# with the above function. However, it first checks to see if the inverse has 
# already been calculated for that particular matrix. 
#If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the matrix and 
# sets the value of the inverse in the cache via the setmean function.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

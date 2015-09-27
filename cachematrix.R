## makeCacheMatrix() great a special "Matrix" and returns a list
## of functions. CacheSolve() returns the invers of the matrix and
## if it has already been computed it gets the inverse from cache

## This function returns a list containing 4 functions similar to
## makeVector() function in the example

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


## If the inverse is not in the cache it computes the inverse using
## solve() function

cacheSolve <- function(x, ...) {
	m<-x$getmatrix()
    	if(!is.null(m)){
      	message("getting cached data")
      	return(m)
    	}
    	matrix<-x$get()
    	m<-solve(matrix, ...)
    	x$setmatrix(m)
    	m
}
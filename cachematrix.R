##The two functions allow the user to cache and retrive the inverse of a Matrix. 
##CacheSolve computes the inverse of the matrix and stores it in the makeCacheMatrix
##function. The inverse can then be retrieved through the getinverse function within
##the makeCacheMatrix function 


## makeCacheMatrix returns a list of four functions to 
##1)set the value of a matrix, 2) get the value of the matrix
##3) set the inverse of the matrix and 4)get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y)
  if(!all(y==x)){
    x <<- y
    m <<- NULL
  }
get<-function() x
setinverse<-function(inverse) i<<-inverse
getinverse<-function() i 
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve first checks if the inverse of the matrix has already been calculated by calling the getinverse() function 
##within the makeCachematrix object. If the inverse has been previously calculated, cacheSolve retrieves the inverse 
##from memory through the getinverse function. If the inverse has not been previously calculate, cacheSolve calculates the inverse
##and caches it by calling the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 i<-x$getinverse()
 if(!is.null(i)){
   message("getting cached data")
   return(i)
 }
 matrix<-x$get()
 i<-solve(matrix)
 x$setinverse(i)
 i
}

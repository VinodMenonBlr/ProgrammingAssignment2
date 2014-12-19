##How to use these functions?
## m <- matrix(c(1,0,2,0,1,0,0,0,1),nrow=3,ncol=3,byrow=TRUE)
## x<-makeCacheMatrix(m)
## x$set(m)
## Gives inverted matrix. Computes it first time. Returns the cached inverse after that
## y<-cacheSolve(x)


##This function object used the notion of function closure supported
##in R to associate state information of matrix objects with the function objects
## using which we can cache data. R maintains reference to the environment at function
## definition which will have the cached objects
makeCacheMatrix <- function(x=matrix()){
  ##matrix to be inversed can be cached here
  ##Assumption the matrix is invertible
  ##reference to cached inverted matrix being initialized to NULL
  gInvMatrix <- NULL
  
  ##set can be used to provide the function to be 
  ##inverted. 
  set <- function(m){
    ##<<- operator is used to assign value to gm which is outside the local 
    ##environment. With this operator R searches the function enclosing environment
    ##finds gm in the function enclosing environment as part of the function closure
    x <<- m
    gInvMatrix <<- NULL
  }
  
  ##function to get the matrix given as input for inversion
  get <- function() x
  
  ##function to set the inverted matrix 
  setInverse <- function(inv){
    gInvMatrix <<- inv
  }
  
  ##function to get the inverted matrix
  getInverse <- function() gInvMatrix
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}

cacheSolve <- function(x,...){
  
  ##check whether there is a cached inverted matrix available.
  ##If so return it. Else compute the inverse and cache
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)){
    print("cached inverted matrix returned")
    return(invMatrix)
  }
  
  #Get the matrix which was given as input
  m <- x$get()
  #Find the inverse of the given matrix object
  invMatrix <- solve(m)
  #Cache it in the cachematrix object
  x$setInverse(invMatrix)
  #return the inverted matrix
  invMatrix
  
}


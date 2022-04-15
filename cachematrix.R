## First Function makeCacheMatrix will create a speical matrix "list" that have the matrix and its inverse
## The second function cacheSolve will check if there is a cached inverse matrix already and if there is so, it will print back. If there is no inverse matrix 
## it will inverse the matrix and store in the speical matrix

## The function makeCacheMatrix will check first of all if the feeded matrix is inversable if yes, it will create a speical matrix
## that contains it and Null space for it's inverse which will be filled later on.

makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x)==nrow(x) && det(x)!=0) { ## Checking if the matrix is inversable
    m<-NULL
    set<-function(y){ ##creating the special matrix
      x<<-y
      m<<-NULL
    }
    get<-function() x 
    setinverse <- function(inverse) m <<- inverse
    getinverse<-function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
  }else{
    return(message("The matrix is'n invertible.")) # reporting that this matrix is no inversable
  }
}



## This function will have an argument as a speical matrix then it will be returned if non, the inverse will be calculated using Solve function
## and store in the speical matrix

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  } else{
    thematrix <-x$get()
    m <- solve(thematrix)
    x$setinverse(m)
  }
  return(m)
}

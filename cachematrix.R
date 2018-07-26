## The function below will creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function() inv<<-solve(x) ## The solve function is applied here, 
                                              ## which can compute the inverse of matrix
        getinverse<-function() inv
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
## If the inverse has not been calsulated, the function below will compute the inverse of the special "matrix".
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        inv
}

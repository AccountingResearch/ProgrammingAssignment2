#Because the computation of a matrix inversion is typically time-consuming, caching the inverse of a matrix instead of computing it repeatedly can be beneficial.  The pair of functions below can be used together to create an inverse matrix cache or retrieve it if it has already been generated.

#The function below creates a special “matrix” object that can cache its inverse, to be returned to the cacheSolve function.

makeCacheMatrix <- function(OriginalMatrix = matrix()) {

#Creating an initial NULL inverse matrix
    InverseMatrix <- NULL

#Setting matrix value
    set <- function(y) {
        OriginalMatrix <<- y
        InverseMatrix <<- NULL
    }

#Getting matrix value
    get <- function() OriginalMatrix

#Setting matrix inverse
    setinverse <- function(inverse) InverseMatrix <<- inverse

#Getting matrix inverse
    getinverse <- function() InverseMatrix

#Returning a list of functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


#The function below calculates the inverse of the special "matrix" returned by the makeCacheMatrix function.  The inverse is retrieved from the cache if it has already been computed.

cacheSolve <- function(x, ...) {

#Getting matrix inverse
    InverseMatrix <- x$getinverse()

# Returning matrix inverse if it exists
    if(!is.null(InverseMatrix)) {
        message("getting cached data")
        return(InverseMatrix)
    }

#Getting matrix
    data <- x$get()

#Calculating matrix inverse
    InverseMatrix <- solve(data, ...)

#Caching matrix inverse
    x$setinverse(InverseMatrix)

#Returning matrix inverse
    InverseMatrix

}
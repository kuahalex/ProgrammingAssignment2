makeCacheMatrix creates a list containing a function to 
1. Set the value of the matrix
2. Get the value of the matrix
3. Set the value of the inverse of the matrix
4. Get the value of the inverse of the matrix


makeCacheMatrix <- function(x=matrix()){
            n <- NULL
            set <- function(y){
                  x <<- y
                  n <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) n <<- inverse
            getinverse <- function() n
            list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

The function below returns the inverse of the matrix, and it checks whether the inverse of the martix has already been calculated. If yes, then it gets result directly and skip the calculation; If not, then the function will calculate the inverse of the martix.

cacheSolve <- function(x, ...) {
          n <- x$getinverse()
          if(!is.null(n)) {
                  message("getting cached data")
                  return(n)
          }
          data <- x$get()
          n <- solve(data, ...)
          x$setinverse(n)
          n

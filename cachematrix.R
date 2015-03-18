makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #set the value of the matrix
        set <- function(y) {
                x <- y
                inv <- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        #get the value of inverse of the matrix
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#function returns the inverse of the matrix
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        #solve returns inverse
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


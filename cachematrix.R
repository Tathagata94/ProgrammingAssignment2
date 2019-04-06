## makechachematrix returns a list of 4 functions namely:
## 1. set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the the matrix
## 4. Get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) inv <<- inverse 
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## Cache Solve checks if the matrix has a cached inverse
## If the cahced inverse is not NULL calculates the inverse using 
## solve() and then returns the inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
}

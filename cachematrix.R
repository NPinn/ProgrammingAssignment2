## These functions set out to create a cache-able matrix, and then
## cache the inverse of the created matrix

## The "makeCacheMatrix" function creates a matrix to be cached

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y){
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) m <<- inverse
		getinverse <- function() m
		list(set = set, get = get,
		     setinverse = setinverse,
		     getinverse = getinverse)
}


## The "cacheSolve" function takes the matrix created from the
## previous function, and returns its inverse

cacheSolve <- function(x, ...) {
		m <- x$getinverse()
		if(!is.null(m)){
			message("getting cached data")
			return(m)
		}
		data <- x$get()
		m <- solve(data)
		x$setinverse(m)
		m
}

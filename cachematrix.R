## This file contains R functions that are able to cache potentially 
## time-consuming computations. For example, calculating the inverse of a matrix
## is typically a costly operation. For a very large matrix, it may 
## take too long to compute the inverse, especially if it has to be computed 
## repeatedly (e.g. in a loop). If the contents of the matrix are not changing,
## it may make sense to cache the value of the inverse so that when we need it 
## again, it can be looked up in the cache rather than recomputed. 
## The code in this file takes advantage of the scoping rules of the R language
## and how they can be manipulated to preserve state inside of an R object.

## makeCacheMatrix creates a special "matrix" which is really a list containing
## a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
            x <<- y
            i <<- NULL
        }	
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix" created with the 
## above makeCacheMatrix function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the
## matrix and sets the value of the inverse in the cache via the setinverse 
## function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
    	message("getting cached matrix inverse")
	return (i)
    }
    matr <- x$get()
    i <- solve(matr, ...)
    x$setinverse(i)
    i
}

## The following two functions first cache the inverse of a matrix 
## “m” and then retrieve the cached inverse if the matrix has not 
## changed. If the matrix has changed or the value of the inverse 
## has not yet been calculated, the “cacheSolve” function 
## calculates a new inverse and returns it.


## The first function, “makeCacheMatrix”, creates a special 
## object, which is really a list containing functions to 
## 1) “set” the value of the matrix “m”; 
## 2) “get” the value of the matrix “m”; 
## 3) set the value of the inverse of “m” (“setinverse”); and 
## 4) get the value of the inverse of “m” (“getinverse”).

## For example: fm <- makeCacheMatrix(m) creates a list “fm” with 
## all four functions for matrix “m”.


makeCacheMatrix <- function(m = matrix()) {

    inv <- NULL
	## Creates “inv” to store the matrix inverse. This is 
	## initialized as null in the “makeCacheMatrix” 
	## environment. 

    set <- function(y) {
        m <<- y
        inv <<- NULL
		## Sets “inv” to null in the global environment.	
    }

    get <- function() m
	## Returns the value of the matrix “m”.

    setinverse <- function(inverse) inv <<- inverse
	## Sets the value of the matrix inverse in the global 
	## environment.

    getinverse <- function() inv
	## Returns NULL if the inverse of the matrix has
	## not been calculated, or if the matrix has changed.

    list(set = set, get = get, setinverse = setinverse, 
    getinverse = getinverse)
	## Returns all four of the functions created by 
	## “makeCacheMatrix”.

}


## The second function, “cacheSolve”, checks whether there is an 
## existing inverse and if so returns the value of the inverse 
## Else, the function calculates the inverse using solve() 
## and returns the newly calculated inverse.


cacheSolve <- function(m, ...) {

    inv <- m$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
		## If the value of “inv” is not NULL, prints a 
		## message and returns the value of the inverse.
    }

    data <- m$get()
    inv <- solve(data, ...)
    m$setinverse(inv)
    inv
	## Else, if the value of “inv” is NULL, calculates the 
	## inverse of the matrix and returns the newly calculated 
	## inverse.

}



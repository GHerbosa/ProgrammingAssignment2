## Two functions that will enable users to cache the inverse of a matrix.

## First function will create a matrix that can cache the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
  	inv<-NULL
 	 set<-function(g){
 	   x<<-g
  	  inv<<-NULL
 	 }
	  get<-function() x
 	 setInverse<-function(solveMatrix) inv<<-solveMatrix
 	 getInverse<-function() inv
 	 list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	}

## Second function will calculate the inverse of the matrix from the first
## function. Take note that if inverse has been calculated already
## while matrix is constant, then it will show the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat, ...)
        x$setInverse(inv)
        inv
}


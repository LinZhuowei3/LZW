##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
	inv <- NULL
	##set the value of the matrix
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	##get the value of the matrix
	get <- function()x
	##set the value of the invertible matrix
	setinverse<-function(inverse) inv <<- inverse
	##get the value of the invertible matrix
	getinverse <- function() inv
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

##This function computes the inverse of the special "matrix"returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed)
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x,...){
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)
	inv
}

##Testing
> a <- c(-2,4,1,-3)
> x <- matrix(a,2,2)
> x
     [,1] [,2]
[1,]   -2    1
[2,]    4   -3
> cache <- makeCacheMatrix(x)
> cache$get()
     [,1] [,2]
[1,]   -2    1
[2,]    4   -3
> cache$getinverse()
NULL
> cacheSolve(cache)
     [,1] [,2]
[1,] -1.5 -0.5
[2,] -2.0 -1.0
> 

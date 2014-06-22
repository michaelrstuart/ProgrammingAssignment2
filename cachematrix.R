
## Coursera R Programming 
## Assignment 2:  Caching the inverse of a matrix.  This program creates a special object which will store a matrix
## and also it's inverse.  

# makeCacheMatrix -- a set of stored functions to assign/retrieve both the original matrix, and its inverse
# Functions within this object:
# getmtrx -- recalls the original matrix passed to the function 
# asgmtrx -- assigns the original matrix 
# getinvr -- recalls the inverse of the original matrix 
# asginvr -- assigns the inverse 

makeCacheMatrix <- function(x = matrix()) {
	
	# local object for the cached inverse of the inbound matrix; set to null at start 
	tmatrixinv <- NULL 

	# cache the inbound matrix 
	asgmtrx <- function(y) { 
		x <<- y 
		tmatrixinv <<- NULL    # push binding to the parent environment 
	}

	# object to return the inbound matrix 
	getmtrx <- function() x

	# object to set the inverse of the inbound matrix 
	asginvr <- function(invmtrx) tmatrixinv <<- invmtrx 

	# object for the inverse of the matrix 
	getinvr <- function() tmatrixinv 

	# package it all together 
	list( getmtrx = getmtrx , 
		  asgmtrx = asgmtrx , 
		  getinvr = getinvr , 
		  asginvr = asginvr 
		  )
}


## cacheSolve --  will return the inverse of a matrix from an object created by makeCacheMatrix() 
## This function first checks to see if the inverse is cached, and will retrieve it if it is. 
## Otherwise, will calculate, cache, and retrieve the inverse

cacheSolve <- function(x, ...) {

	# pull back the cached inverse of the matrix 
	invmtrx <- x$getinvr() 

	# is the inverse already cached
	if (!is.null(invmtrx)) { 
		message("Inverse returned from cache")
		return(invmtrx)
	}

	# Otherwise, let's calculate the inverse
	tmpinverse <- solve(x$getmtrx())

	# Assign the inverse to the inverse cache object 
	x$asginv(tmpinverse)  

	# Return it to the function call 
	tmpinverse 
}

# # Scratch area 

mvect <- sample(1:500, 25)

tmatrix <- matrix(mvect, nrow=5, ncol=5 )

cmatrix <- makeCacheMatrix(tmatrix)
cmatrix$getmtrx() 
cmatrix$getinvr()

cacheSolve(cmatrix)
cacheSolve(cmatrix)



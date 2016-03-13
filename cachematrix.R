## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL #set initial cached inverse matrix to null
	
	set <- function(in_data){
		x <<- in_data #set new in_data
		inverseMatrix <<- NULL #if new data -> inversecache should be nulified  
	}
	get <- function() x #transform data to function
	setInvM <- function(in_inverseMatrix) inverseMatrix <<- in_inverseMatrix
	getInvM <- function() inverseMatrix
	
	list(set = set,
	     get = get,
	     setInvM = setInvM,
	     getInvM = getInvM
	     ) #print methods on autoprint
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInvM() #if cached return cached
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
		data <- x$get()#get data through get interface

        inverseMatrix <- solve(data,...) #solve
        x$setInvM(inverseMatrix) #store through setInvM interface
        inverseMatrix
}

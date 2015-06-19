## makeCacheMatix function contains 4 functions:
## 1- it sets/chnages the value for a matrix, 2- it gets the value of the matrix, 
## 3- it sets the value of the inverse matrix,4- it gets the value of inverse matrix

makeCacheMatrix <- function(m = matrix()) {
       
        ## initilize the inverse matrix variable "inv" with NULL 
        inv <- matrix()
        inv <- NULL         
        ## setmatrix function sets/changes the value of matrix stored in the main function
        ## it reinitilizes "inv" matrix (which will hold the inverse matrix) with NULL.
        setmatrix <- function (y) { 
                m <<- y 
                inv <<- NULL     
        }            
        
        ## getmatrix returns the matrix m stored in the main function. It doesn't require any input.
        getmatrix <- function() m   
        
        ## setinverse and getinverse functions don't calculate the inverse matrix.
        ## setinverse funciton stores the value of the input into a matrix variable "inv" in the main function. 
        ## getinverse returs the inverse matrix. 
        setinverse <- function (inverse = matrix())    inv <<- inverse                                                                    
        getinverse <- function() inv
        
        ## The funciton list store 4 functions above in the function makeCacheMatrix
        list( setmatrix = setmatrix, getmatrix = getmatrix,
              setinverse= setinverse, getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If inverse matrix found, it will retrun the inverse matrix from cache. 
## Otherwise, it will calculate the inverse matrix and stores it in matrix "inv" and return it.

cacheSolve <- function(x, ...) {
        ## get the inverse matrix and save it in "inv" matrix variable
        inv <- x$getinverse()
        ## Check to see if cached Inverse matrix exists or the inverse needs to be computed.
        ## If the "inv" is not equal to NULL, that means the cached inverse matrix exists and 
        ## there has been no change to the matrix.Get the inverse matrix from the cache and retrun it.
        ## Otherwise,compute the Inverse matrix using solve funciton.
                if (!is.null(inv)) {
                        message ("getting cached data")
                        return(inv) 
                }
        ## The lines below will be executed,only if cached inverse martix does not exist.
        ## Get the input matrix and save it to "inmatrix" variable
        inmatrix <- x$getmatrix()
        ## Calculate the inverse matrix using solve function
        inv <- solve(inmatrix)
        ## Set/save the value of inverse matrix in "Inv"
        x$setinverse(inv)
        ## Return a matrix that is the inverse of input/original matrix
        inv
 }

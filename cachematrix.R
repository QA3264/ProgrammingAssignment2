## makeCacheMatix creates a special 'matrix'. It contains 4 functions
## 1- sets the value for the matrix, 2-get the value matrix, 3-set the inverse matrix, 
## and 4-gets the inverse matrix

makeCacheMatrix <- function(m = matrix()) {
       
        ## initilize the inverse matrix variable "inv" with NULL 
        inv <- matrix()
        inv <- NULL         
        ## setmatrix function changes the matrix stored in the main function and reinitilizes "inv"
        ##matrix with NULL.
        setmatrix <- function (y) { 
                m <<- y 
                inv <<- NULL     
        }            
        ## getmatrix returns the matrix m stored in the main function. Doesn't require any input.
        getmatrix <- function() m   
        ##setinverse and getinverse don't calculate the inverse matrix.
        ##setinverse stores the value of the input into a matrix variable "inv" in the main function. 
        ##getinverse returs the inverse matrix. 
        setinverse <- function (inverse = matrix())    inv <<- inverse                                                                    
        getinverse <- function() inv
        
        ## Storing of 4 functions above in the function makeCacheMatrix
        list( setmatrix = setmatrix, getmatrix = getmatrix,
              setinverse= setinverse, getinverse = getinverse)
}

##The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##if inverse found, it will retrun the inverse matrix from cache. Otherwise, it will calculate the inverse
##matrix and stores it in matrix "inv" and return it.

cacheSolve <- function(x, ...) {
        ## get the inverse matrix and save it in "inv" matrix variable
        inv <- x$getinverse()
        ##check if the inverse matrix already exists.
        ##check if the "inv" is not equal to NULL. If not NULL (that means inverse already exists).
        ##Then get the inverse from the cache
                if (!is.null(inv)) {
                        message ("getting cached data")
                        return(inv) 
                }
        ##get the input matrix and save it "inmatrix" variable
        inmatrix <- x$getmatrix()
        ##Calculate the inverse matrix using solve function
        inv <- solve(inmatrix)
        ##set the value of inverse
        x$setinverse(inv)
        ## Return a matrix that is the inverse of input/original matrix
        inv
 }

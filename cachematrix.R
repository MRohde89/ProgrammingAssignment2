## On one hand makeCacheMatrix creates a special matrix which can be used to 
## assign a value to an object in an environment that is different from the current
## environment. Below are two functions that are used to create a special object that 
## stores a matrix and caches its inverse matrix

## This function creates a list with 4 functions.
## set function: let you change the original matrix, but you must input a matrix or it will
##               raise an error)
## get function: output your matrix
## setinverse:   let you input the inverse-matrix. You can input all kind of matrices. 
##               But still it will raise an error if the input is not a matrix
## getinverse:   return the values which is saved as the inverse matrix (default = 0)

makeCacheMatrix <- function(x = matrix()) {
        inverse = NULL
        set <- function(y = matrix()) {
                if (class(y) == "matrix") {
                        x <<- y
                        inverse <<- NULL
                        
                } else {
                        warning("This is not a Matrix. No changes were made.")
                        
                        
                }
        }
        get <- function() x
        setinverse <- function(inv_Matrix = matrix()) {
                
                if (class(inv_Matrix) == "matrix") {
                        inverse <<- inv_Matrix  
                } else {
                        warning("This is not a Matrix. No changes were made.")
                }
                
        }
        getinverse <- function() inverse
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function takes a makeCacheMatrix object and returns either a cached matrix 
## or it will calculate the inverse matrix and return the result
## However, since our task was to only handle matrices with an inverse-matrix, the function
## will raise an error and abort if there is no inverse.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting chached data")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix,...)
        x$setinverse(inverse)
        inverse

}

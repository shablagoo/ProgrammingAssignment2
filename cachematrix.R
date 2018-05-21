## 'makeCacheMatrix' takes a matrix in as a input and sets its values, 
## it will then fetch the values so that they can be solved in 'cachesolve'.
## The function also sets a NULL value for s (the inverse of the matrix), and
## caches/prints the calculated final inverse values.If values already exist,
## the existing cached values are printed. 

makeCacheMatrix <- function(data = matrix()) {
        s <- NULL
        
        ## Function setting the matrix.
        
        set <- function(temp) {
                data <<- temp
                s <<- NULL
        }
        
        ## getting the matrix for 'cachesolve'
        
        get <- function() data
        
        ## Functions for defining the matrix inverse, and retrieving
        ## the matrix inverse.
        
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
              
}

## cachesolve determines if there is existing cached values for the matrix
## inverse, and if there is not it solves for the inverse of the matrix. 

cachesolve <- function(data, ...) {
        
        ## Checking getsolve to determine if there is an existing cached 
        ## matrix inverse. Retrieving value if it exists.
        
        s <- data$getsolve()
        
        if(!is.null(s)) {
                message("Getting cached matrix data")
                return(s)
        }
        
        ## Performing inverse operation on the matrix data and setting 
        ## the solution.
        
        Matrix <- data$get()
        s <- solve(Matrix, ...)
        data$setsolve(s)
        
        ## Printing the matrix inverse
        
        print(s)
        
}

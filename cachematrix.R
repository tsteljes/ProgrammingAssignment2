source("cachematrix.R")

## The following pair of functions creates an invertible matrix that can be cached
## as a way to avoid repeated computing and costs. There should be some understanding of:
## 1) how "lexical closures or function closures)" work, i.e. technique to implement lexical scoping and nesting functions
##    that have access to x as within the "lexical scope" of x.
## 2) creating an invertible square matrix to allow for testing a few examples of square matrices (out-of-scope but useful)

## The first function, makeCacheMatrix, sets up a special "matrix" object that is an
## invertible square matrix. FYI: A square matrix that does not have an inverse is referred to as "singular".
## If the square matrix (i.e. equal number of rows & columns) set up is not invertible, 
## the second function that "solves" the matrix (data) will give an error "...system is exactly singular..." 
## To test if matrix is invertible : you may wish to run your square matrix and evaluate separately
## by solve() function; or use the Sarrus rule to calculate determinant in the sample matrix is > 0.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(yyy) {                                          ## Name of local variable (yyy) does not matter in lexical scoping 
                x <<- yyy
                m <<- NULL                                              ## Initializing using superassignment operator
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)                                ## Sets and gets the solve function for subsequent calls 
}


## The second function, cacheSolve,  uses the solve() function in R to compute the inverse of a 
## invertible square "matrix" from makeCacheMatrix. It first checks to see if the inverse matrix has already been
## cached, if so, produces the message confirming the matrix has not changed, and returns the cached matrix. 
## Otherwise, it goes on to cache a new inverse matrix and returns the newly cached inverse matrix (m).

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        ## Returns a matrix that is the inverse of 'x' matrix
}

## Test 1, square matrix 2X2.
## > my_2X2 <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2))
## > cacheSolve(my_2X2)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > cacheSolve(my_2X2)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Test 2, square matrix 3X3.
## > my_3X3 <- makeCacheMatrix(matrix(c(1,2,4,2,1,3,1,4,1), nrow=3))
## > cacheSolve(my_3X3)
##            [,1]        [,2]       [,3]
## [1,] -0.5789474  0.05263158  0.3684211
## [2,]  0.7368421 -0.15789474 -0.1052632
## [3,]  0.1052632  0.26315789 -0.1578947
## > cacheSolve(my_3X3)                                                          
## getting cached data
##            [,1]        [,2]       [,3]
## [1,] -0.5789474  0.05263158  0.3684211
## [2,]  0.7368421 -0.15789474 -0.1052632
## [3,]  0.1052632  0.26315789 -0.1578947

## Both my_2X2 and my_3X3 are what are known as closures; they are a special kind of object that combines both the function
## and the environment in which they were created. The environment contains any local variables that were in-scope at the 
## time the closures were created. 
## References:
## https://darrenjw.wordpress.com/2011/11/23/lexical-scope-and-function-closures-in-r/
## Hadley's http://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r


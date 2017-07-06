## Put comments here that give an overall description of what your
## functions do
## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function
## which is  a list containing a function to

    # set the value of the matrix: set <- function(y) 
    # get the value of the matrix: get <- function() x
    # set the value of the inverse of the matrix: setinverse
    # get the value of the inverse of the matrix: getinverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

#examples
# A<- matrix (c(1,2,3,4),nrow=2,byrow=TRUE)
#A
#      [,1] [,2]
#[1,]    1    2
#[2,]    3    4
#AC<- makeCacheMatrix(A)
#cachesolve(AC)
#     [,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5

#B<-matrix(c(5,6,7,8,9,10,11,12,14),ncol=3,byrow=FALSE)
#B
#      [,1] [,2] [,3]
#[1,]    5    8   11
#[2,]    6    9   12
#[3,]    7   10   14
#BC=makeCacheMatrix(B)
#cachesolve(BC)
#      [,1]       [,2] [,3]
#[1,]   -2  0.6666667    1
#[2,]    0  2.3333333   -2
#[3,]    1 -2.0000000    1
#cachesolve(AC)
#getting cached data
#      [,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#cachesolve(BC)
#getting cached data
#     [,1]       [,2] [,3]
#[1,]   -2  0.6666667    1
#[2,]    0  2.3333333   -2
#[3,]    1 -2.0000000    1

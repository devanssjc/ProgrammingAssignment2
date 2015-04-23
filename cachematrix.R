##
## -----------------------------------------------------------------------
## The two functions that follow, makeCasheMatrix, and cacheSolve,
## were generated for Programming Assignment #2 for R Programming
## course. After these functions, two additional functions, not part
## of the assignment, are includes. These are mlfa and sfa. mlfa
## is a multifunction test program that was used to validate the
## assignment worked correct. sfa was a single function version of
## the assignment that was used to work out the matrix operations
## in a non-cached flat environment. Agan, mlfa and sfa are not
## part of this assignment.
## -----------------------------------------------------------------------
##
## -----------------------------------------------------------------------
##  This function, makeCacheMatrix, is used to set 4 functions:
##  set - this sets the value of the input square matrix
##  get - this gets the value of the input square matrix
##  setinverse - this sets the value of the inverse matrix
##  getinverse - this gets the value of the inverse matrix
##  i is forced to be in the Global Environment so that it can be
##  read by other functions. This function returns set, get, setinverse,
##  and getinverse as functions. i is set to NULL before the inverse
##  is computer, and then i is equal to the inverse after the inverse
##  is calculated.
## -----------------------------------------------------------------------
##
makeCacheMatrix <- function(x = matrix()) {
    i <<- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
##
## -----------------------------------------------------------------------
## This function, cachesolve, functions in two ways, cached and uncashed
## to return the inverse of the matrix x (passed). i will have been set
## to NULL by makeCacheMatrix the first time this function is called,
## and that will in turn cause this function to perform the functions
## below the else statement (getting the data, performing the solve 
## function to get the inverse, and then saving the inverse matrix.
## i is reset from NULL to a value this activity, so subsequent
## calls to this function will result in the if logic evaluating to
## true, allowing for the direct return of i (the inverse)
## -----------------------------------------------------------------------
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)}
    else message("caching data")
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## ----------------------------------------------------------------------
## The code below this line is not part of the formal assignment
## However, it is used to test the two functions above
## mlfa is a multi-level test function. It sets up various matrices
## sufficient to create full n x n matrices and solutions, of the form
## [xdata] x [adata] = [bdata]
##
## ainverse holds the solution from the solve function
## xsolve holds the computed value from the matrix multiplicaiton
## [ainverse] x [bdata] = [xsolve]
## 
## Comparing [xdata] to [xsolve] shows that the inverse was calculated 
## correctly. Timing parameters t0 through t5 are used to time 3 cases
## The first case is new data, requiring a new calculation
## The second cases is old data, not requiring a new calculation
## The third case resets the cache dirty flag (i) and initiated a new
## calculation.
##
## The random number seed is initialized at the beginning to allow for
## repeatible results. matrix_size is a variable for convenience.
## -----------------------------------------------------------------------
# Multi-function assignment - test
mlfa <- function (){
    set.seed(542707)
    matrix_size <- 1000
    
    xdata <- matrix("NULL", nrow = matrix_size, ncol = matrix_size)
    ainverse <- matrix("NULL", nrow = matrix_size, ncol = matrix_size)
    ainverse <- matrix("NULL", nrow = matrix_size, ncol = matrix_size)
    i <<- matrix("NULL", nrow = matrix_size, ncol = matrix_size)
    xsolve <- matrix("NULL", nrow = matrix_size)
    adata <- matrix("NULL", nrow = matrix_size)
    bdata <- matrix("NULL", nrow = matrix_size)
    
    xdata <- matrix(rnorm(matrix_size), nrow = matrix_size)
    adata <- matrix(rnorm(matrix_size^2), nrow = matrix_size, ncol = matrix_size)
    bdata <- adata %*% xdata    

    print("original solution")
    print(xdata[1:10,1])
    
    z <- makeCacheMatrix(adata)
    t0 <- proc.time()
    ainverse <- cacheSolve(z)
    t1 <- proc.time()
    print(t1-t0)
   
    t2 <- proc.time()
    ainverse <- cacheSolve(z)
    t3 <- proc.time()
    print(t3-t2)
    
    i <<- NULL
    t4 <- proc.time()
    ainverse <- cacheSolve(z)
    t5 <- proc.time()
    print(t5-t4)
    
    
    xsolve <- ainverse %*% bdata
    print("new solution")
    print(xsolve[1:10,1])
}
## -----------------------------------------------------------------------
## The code below was used to test a flat version of the assignment
## This code is not part of the assignment, but is maintained for
## future reference only.
## -----------------------------------------------------------------------
# Single function (flat assignment) - test
sfa <- function() {
    set.seed(542707)
    matrix_size <- 1000
    adata <- matrix(rnorm(matrix_size^2), nrow = matrix_size, ncol = matrix_size)
    ainverse <- matrix("NULL", nrow = matrix_size, ncol = matrix_size)
    xdata <- matrix(rnorm(matrix_size), nrow = matrix_size)
    xsolve <- matrix("NULL", nrow = matrix_size)
    bdata <- matrix("NULL", nrow = matrix_size)
    bdata <- adata %*% xdata
    # print(adata)
    # print(ainverse)
    print(xdata[1:10])
    # print(xsolve)
    # print(bdata)
    t0 <- proc.time()
    ainverse <- solve(adata)
    xsolve <- ainverse %*% bdata
    t1 <- proc.time()
    t2 <- proc.time()
    print(t1 - t0)
    # print(t2 - t1)    
    print(xsolve[1:10])
    # print("Done ")
    # print(ainverse)
    # print(xdata)
}

## -----------------------------------------------------------------------
## The code below this point is not part of the assignment. It demonstrates
## the example provided as part of the assignment.
## -----------------------------------------------------------------------
makeVector <- function(x = numeric()) {
    m <<- NULL
    set <- function(y) {
       x <<- y
       m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
        setmean = setmean,
        getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)}
    else message("caching data")
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

exampletest <- function() {
    set.seed(542707)
    z <- makeVector(rnorm(20000000))
    t0 <- proc.time()
    print(cachemean(z))
    t1 <- proc.time()
    print(t1 - t0)
    t2 <- proc.time()
    print(cachemean(z))
    t3 <- proc.time()
    print(t3 - t2)
    m <<- NULL
    t4 <- proc.time()
    print(cachemean(z))
    t5 <- proc.time()
    print(t5 - t4)
}

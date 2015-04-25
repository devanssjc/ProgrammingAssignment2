##
## -----------------------------------------------------------------------
## The two functions that follow, makeCasheMatrix, and cacheSolve,
## were generated for Programming Assignment #2 for R Programming
## course. After these functions, two additional functions, not part
## of the assignment, are included. These are mlfa and sfa. mlfa
## is a multifunction test program that was used to validate the
## assignment functions work correct. sfa was a single function version of
## the assignment that was used to work out the matrix operations
## in a non-cached flat environment. Again, mlfa and sfa are not
## part of this assignment.
## -----------------------------------------------------------------------
##
## -----------------------------------------------------------------------
## This function, makeCacheMatrix, is used to create 4 objective functions:
## set - this sets the value of the input square matrix
## get - this gets the value of the input square matrix
## setinverse - this sets the value of the inverse matrix
## getinverse - this gets the value of the inverse matrix
##
## The functions work with variables local to the makeCasheMatrix. These are:
## a - the square matrix from which an inverse will be calculated
## i - the inverse matrix calculated from a
##  
## Setting i = NULL serves as a flag that the inverse needs to be calculated
## So this is why calling makeCacheMatrix and set both set i to NULL. Also
## note that there are statements temp = a[1]. These statements are not
## required for correct operation of the function. These were added just to
## force R to evaluate a immediately (avoid R lazy evaluation feature)
##
## -----------------------------------------------------------------------
##
makeCacheMatrix <- function(a = matrix()) {
    i <- NULL
    temp <- a[1]
    set <- function(a) {
        i <<- NULL
        temp <- a[1]
    }
    get <- function() a
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
##
## -----------------------------------------------------------------------
## This function, cacheSolve, returns the inverse matrix of a square matrix which 
## was set up previously by calls to makeCacheMatrix and possibly calls to set,
## an objective function defined in makeCacheMatrix.
##
## This function executes along two paths, cached and uncached,
## depending upon whether the inverse has been calculated previously or
## not. The function uses the variable containing the inverse matrix (i2)
## as the status of cache. If i2 is NULL (from a call of$getinverse)
## it knows it must compute the inverse, othersise this function simply
## returns i2.
##
## This coding of this function more or less follows the example
## provided in the course. However, to avoid confusion, I renames'
## the variables so they do not match those in makeCacheMatrix.
## This is not important, but it helps clarify that the variable i2
## used here is not the same variable i used in makeCacheMatrix.
## i2 only matches i due t the call to and assignment to of$getinverse().
## 
## of is short for objective funtions.
## -----------------------------------------------------------------------
##
cacheSolve <- function(of, ...) {
    i2 <- of$getinverse()
    if(!is.null(i2)) {
        message("getting cached data")
        return(i2)}
    else message("caching data")
    data <- of$get()
    i2 <- solve(data, ...)
    of$setinverse(i2)
    i2
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
## Comparing [xdata] to [xsolve] should show that the inverse was calculated 
## correctly. Timing parameters t0 through t3 are used to time 2 cases
## The first case is new data, requiring a new calculation
## The second cases is old data, not requiring a new calculation
## The second case should be much fast than the first case.
##
## The random number seed is initialized at the beginning to allow for
## repeatible results. matrix_size is a variable for convenience.
## 
## z is used as a container for the objective functions created in 
## makeCacheMatrix, which is passed to cacheSolve.
##
## -----------------------------------------------------------------------
# Multi-function assignment - test
mlfa <- function (){
    set.seed(542707)
    matrix_size <- 1000
##
    xdata <- matrix("NULL", nrow = matrix_size, ncol = matrix_size)
    ainverse <- matrix("NULL", nrow = matrix_size, ncol = matrix_size)
    ainverse <- matrix("NULL", nrow = matrix_size, ncol = matrix_size)
    xsolve <- matrix("NULL", nrow = matrix_size)
    adata <- matrix("NULL", nrow = matrix_size)
    bdata <- matrix("NULL", nrow = matrix_size)
##
    xdata <- matrix(rnorm(matrix_size), nrow = matrix_size)
    adata <- matrix(rnorm(matrix_size^2), nrow = matrix_size, ncol = matrix_size)
    bdata <- adata %*% xdata    
##
    z <- makeCacheMatrix(adata)
    t0 <- proc.time()
    ainverse <- cacheSolve(z)
    t1 <- proc.time()
    print(t1-t0)
##
    t2 <- proc.time()
    ainverse <- cacheSolve(z)
    t3 <- proc.time()
    print(t3-t2)
##
    print("")
    print("original solution")
    print(xdata[1:10,1])
    xsolve <- ainverse %*% bdata
    print("")
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
## the example provided as part of the assignment, with a few improvements.
## One improvement I made was the 'set' function. I got rid of the extra
## assignment and transistion variable y. x is passed directly. Then, I also
## added a dummy assignment for temp to force x to be evaluated immediately.
## I discovered the R has lazy evaluation, and this is not needed here, so 
## the assignment forces the evaluation of x. The call to the exampletest test
## the example, timing the first call and the second call to cachemean to
## see that the two branches are taken and the second call is much faster,
## because it is cached.
## -----------------------------------------------------------------------
makeVector <- function(x = numeric()) {
    temp <- x[1]
    m <- NULL
    set <- function(x) {
       temp <<- x[1]
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
    m1 <- x$getmean()
    if(!is.null(m1)) {
        message("getting cached data")
        return(m1)}
    else message("caching data")
    data <- x$get()
    m1 <- mean(data, ...)
    x$setmean(m1)
    m1
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
}
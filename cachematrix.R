## Below are "a pair of functions that cache the inverse of a matrix".
## Note: the second function (cacheSolve) is superfluous, but I kept it according to the 
## course assignment.

## The following function creates an "object" having two "instance variables"
## (the got matrix, x and its inverse, i) and their "getter methods" + 1 setter method...
## Inverse is calculated lasily, i.e. only when it is requested
makeCacheMatrix <- function(x = matrix()) {
    ## no validation of x is needed, because the task specification states:
    ## "the matrix supplied is always invertible"...
    ## Note: solve(matrix()) ~works, so we are OK with default x too...

    i <- NULL # let's initialize the inverse matrix

    getOrig <- function() {x}
  
    setOrig <- function(y) {
        if (identical(x, y))
            message("setOrig is called with unchanged value -->  keep inverse!")
        else {
            x <<- y
            i <<- NULL }
        x} # this solution
        ## keeps the already calculated inverse, if possible. In other words: if y is the same 
        ## as x, then we don't initialize i: if it already contains a calculated inverse,
        ## then that inverse is kept...
  
    getInverse <- function(...) {

        ## we'll return i, but first we ensure that it contains a fine value:
        if (is.null(i)) {
            message("no cached inverse --> it has to be calculated now")
            i <<- solve(x, ...)} ## "<<-" makes the trick of this assignment: otherwise
            ## the result of solve() would be stored in a local variable of getInverse()
            ## and would be lost when getInverse() returns...
        i}
  
    ## note: setInverse is not provided as it is not a must for the assignment AND
    ## it is hard to implement correctly and efficiently: if it accepts ANY value,
    ## then calling it with wrong parameters can (silently!) make x and its inverse
    ## incosistent: imagine if we call setInverse() with a matrix that is
    ## NOT the inverse of x...
  
    list (getOrig = getOrig, setOrig = setOrig, getInverse = getInverse)}

## The following function computes the inverse of the special "matrix" (object)
## returned by makeCacheMatrix above and returns it.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) x$getInverse(...)

##################################### Test cases follow. Run them in the console...

## > xx = makeCacheMatrix(matrix(data=1:4,nrow=2))
## > xx$getOrig()
## > xx$getInverse()
## > xx$getInverse() -- call it again: no calculation (message) should happen!
## > xx$getOrig() %*% cacheSolve(xx) -- unity matrix should be printed
## > xx$getOrig() %*% xx$getInverse() -- same as previous...
## > xx$setOrig(matrix(data=1:4,nrow=2)) -- no change of x, so
##            -- calculated inverse should remain! Check it: see the message +
## > xx$getInverse() -- lack of message indicates reuse of existing inverse!
## > xx$setOrig(matrix(c(5,4,3,2,1,1,1,1,1),nrow=3)) -- check if setOrig works well...
## > xx$getOrig() -- is it the new 3x3 matrix?
## > xx$getInverse()
## > xx$getOrig() %*% cacheSolve(xx) -- interesting result: rounding errors are seen...
## > xx = makeCacheMatrix() -- check default value...
## > xx$getOrig()
## > xx$getInverse()

##################################### Note about OOP

## Above we imitate a key concept of "object oriented programming" (OOP):
## our functions behave as they were "methods" (member functions) of a class (object).
## That class would have data members (private "instance variables") -- the one we use
## for caching and the original matrix.
## See e.g. https://en.wikipedia.org/wiki/Member_variable and 
## https://en.wikipedia.org/wiki/Class_%28computer_programming%29
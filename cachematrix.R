# this set of functions provides a means to use a
# matrix and its inverse with a cached result for the inverse

# FIRST PASS: using a methodology similar to the example at
# https://class.coursera.org/rprog-030/human_grading/view/courses/975104/assessments/3/submissions
# which may be accepted as the "more R"-way to solve this problem, in much
# the same way that some solutions in Python are "more Pythonic". I'm not
# seasoned enough in R to tell.


# creates a matrix wrapper which allows setting and getting
# a cached version of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(newInverseValue) cachedInverse <<- newInverseValue
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}

# provides the inverse for a matrix x, either by calculating
# and caching it or by pulling it out of the cache
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    result <- x$getInverse()
    if (!is.null(result)) {
        print('returning cached result')
        return(result)
    }
    inverse <- solve(x$get())
    x$setInverse(inverse)
    inverse
}

# SECOND PASS:
# Having worked extensively in some 20 other programming languages of which 14
# could either be considered truly Object-oriented (Javascript, PHP, Tcl (with ITcl),
# Delphi, Perl, Python, C++, VB.NET, C#, Ruby), or perhaps "close-enough"
# to Object-oriented (eg VB6) or can be used in an OO fashion (C with structs, function
# pointers and explicit provision of the 'this' variable), I believe there is a cleaner way. 
#
# In this method, cacheSolve essentially becomes a method (getInverse)
# on the wrapped object which comes back from makeCacheMatrix (now makeSelfCachingMatrix)
# such that the caller # doesn't need to know to explicitly solve and cache the result. 
# The inverse is calculated lazily (ie on first request) and the cached result is returned 
# until the the matrix is replaced -- wherupon the next call recalculates.

makeSelfCachingMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        cachedInverse <<- NULL
    }
    get <- function() x
    getInverse <- function() {
        if (!is.null(cachedInverse)) {
            return(cachedInverse)
        }
        cachedInverse <<- solve(x)
        cachedInverse
    }
    list(get = get, set = set, getInverse = getInverse)
}

# THIRD PASS:
# A truly OO language would have set up a class with methods, encapsulating the
# functionality above into a clean bit of code which is easier to work with. 
# R, unfortunately, has a bit of a history here, between S3, S4 and R5 classes. I've been (so far)
# unable to unravel the most-accepted method to use between them. S3 seems simpler, S4 is
# recommended often and R5 is the new kid on the block (and feels more natural to me, as
# a non-seasoned R programmer). With R5, we could do something like:

SelfCachingMatrix <- setRefClass('SelfCachingMatrix', 
                                 fields=list(actual='matrix', cachedInverse='matrix'),
                                 methods=list(inverse=function() {
                                                    if (length(cachedInverse) != 0) {
                                                        # uncomment below to verify value pulled from cache
                                                        #print('cached')
                                                        return(cachedInverse)
                                                    }
                                                    # uncomment below to verify cache/calculation
                                                    #print('calculated')
                                                    cachedInverse <<- solve(actual)
                                                    return(cachedInverse)
                                                },
                                                set=function(newValue) {
                                                    actual <<- newValue
                                                    cachedInverse <<- matrix(numeric(0))
                                                }))
### Remove first column of comments to test-run the SelfCachingMatrix or copy lines below
### into your RTerm session
### and we'd use this like
#selfCached <- SelfCachingMatrix$new(actual=matrix(c(1,2,3,4), nrow=2, ncol=2))
### calculates the first time:
#firstInverse <- selfCached$inverse()
### next call comes from the cache:
#firstCachedInverse <- selfCached$inverse()
### set method changes the internal matrix, resetting cache:
#selfCached$set(matrix(c(2,1,1,2), nrow=2, ncol=2))
### so this call is calculated
#secondInverse <- selfCached$inverse()
### and this one is cached
#secondCachedInverse <- selfCached$inverse()


## makeCacheMatrix creates a special "matrix" 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m_inv <<- inv
        getinv <- function() m_inv
        list(set = set, get=get,
             setinv = setinv,
             getinv = getinv)
}

## the cacheSolve function calculates the inverse of the special "matrix" 
## created by the makeCacheMatrix function above
## 1. It checks to see if the inverse has already been calculated. If so,
## 2A. it gets the inverse from the cache and skips computing the inverse
## 2B. Otheriwse, it calculates the matrix inverse using the solve function
## and sets the value of the inverse in the cache using the setinv function

cacheSolve <- function(x, ...) {
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- x$get()
        m_inv <-solve(data)
        x$setinv(m_inv)
        m_inv
}

## Trial run:
## > x <- matrix(c(4,2,7,6), 2, 2)
## > trial <- makeCacheMatrix(x)
## > trial$get()
##      [,1] [,2]
## [1,]   4    7
## [2,]   2    6
## getting the matrix inverse
## > cacheSolve(trial)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## in the first run, cache is empty
##
## second run:
## > cacheSolve(trial)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

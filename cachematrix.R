## Put comments here that give an overall description of what your
## functions do
## The functions create a matrix and It's inverse and the result is stored in cache

## Write a short comment describing this function
## Function that creates a "matrix" object and store in cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  
}


## Write a short comment describing this function
## Function that computes the inverse of the "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated and It has not changed, 
## then the cachesolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("data exists in cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Test
## 1st step
## First Matrix
## > m1 <- rbind(c(1/3,-7/8,9/16),c(7/8,-9/16,1/3),c(9/16,-1/3,7/8))
## > m1
## [,1]       [,2]      [,3]
## [1,] 0.3333333 -0.8750000 0.5625000
## [2,] 0.8750000 -0.5625000 0.3333333
## [3,] 0.5625000 -0.3333333 0.8750000

## 2nd step
## Cache the m1
## > m2 = makeCacheMatrix(m1)

## 3rd step
## Get the m2 values
## > m2$get()
## [,1]       [,2]      [,3]
## [1,] 0.3333333 -0.8750000 0.5625000
## [2,] 0.8750000 -0.5625000 0.3333333
## [3,] 0.5625000 -0.3333333 0.8750000

## 4th step
## Solve the cached matrix (first time)
## > cacheSolve(m2)
## [,1]        [,2]       [,3]
## [1,] -0.97027743  1.47199263 0.06299068
## [2,] -1.47199263 -0.06299068 0.97027743
## [3,]  0.06299068 -0.97027743 1.47199263

## 5th step
## Solve the cached matrix (second time)
## > cacheSolve(m2)
## data exists in cache
## [,1]        [,2]       [,3]
## [1,] -0.97027743  1.47199263 0.06299068
## [2,] -1.47199263 -0.06299068 0.97027743
## [3,]  0.06299068 -0.97027743 1.47199263

## The data already exists in cache

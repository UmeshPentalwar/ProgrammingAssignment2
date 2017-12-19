## Creating a funnction to set the matrix and to cache its inverse 
## This function returns a list of functions which cache the matrix and its inverse

makeCacheMatrix  <- function(x = matrix()) {
  invr <- NULL
  setmatrix <- function(m) {
    x <<- m
    invr <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inv.mat) invr <<- inv.mat
  getinverse <- function() invr
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Creating function to calculate the inverse of the matrix returned by "makecacheMatrix" function
## and to cache its inverse.It calculates inverse only if the matrix is changed,for the same matrix
## it retrives the inverse from the cache .

cacheSolve <- function(x, ...) {
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  new.mat <- x$getmatrix()
  invr <- solve(new.mat)
  x$setinverse(invr) 
  invr 
}

## Testing the functions from console for the arbitary data 

> source('makeCacheMatrix.R')
> source('cacheSolve.R')
> m <- matrix(rnorm(9,5,5),3,3)
> spl.matrix <- makeCacheMatrix() 

## seting the matrix using setmatrix function and printing it out 
> spl.matrix$setmatrix(m)
> spl.matrix$getmatrix()
[,1]       [,2]      [,3]
[1,] 3.240713 -8.2039690 13.644451
[2,] 1.058862  0.2572863  5.902487
[3,] 6.911552 13.0452119  4.233917

##check for the inverse if it already exists 
> spl.matrix$getinverse()
NULL

##calculate the matrix inverse using "cacheSolve" function and check for the cached inverse 
> cacheSolve(spl.matrix)
[,1]       [,2]        [,3]
[1,]  0.19992180 -0.5602605  0.13677812
[2,] -0.09563451  0.2122304  0.01232739
[3,] -0.03169580  0.2606756 -0.02507431
> spl.matrix$getinverse()
[,1]       [,2]        [,3]
[1,]  0.19992180 -0.5602605  0.13677812
[2,] -0.09563451  0.2122304  0.01232739
[3,] -0.03169580  0.2606756 -0.02507431

##again invoke the same funtion to get inverse and check if it uses the cached value for inverse 
> cacheSolve(spl.matrix)
getting cached data
[,1]       [,2]        [,3]
[1,]  0.19992180 -0.5602605  0.13677812
[2,] -0.09563451  0.2122304  0.01232739
[3,] -0.03169580  0.2606756 -0.02507431





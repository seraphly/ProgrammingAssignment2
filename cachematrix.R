## Below are two functions that are used to
## create a special object that stores a numeric (square) matrix and caches its inverse


## First, creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Then, compute the inverse of this special "matrix" created with the above function.
## However, it first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse from cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix, and 
## sets the value of inverse in the cache via setinv function.
## Note: Assuming the input matrix is a square matrix

cacheSolve <- function(x,...) {
  inv <- x$getinv()
  
  if (!is.null(inv)){
    print("getting cached data")
    return (inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

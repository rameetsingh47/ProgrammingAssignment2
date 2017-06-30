## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse
 

-makeCacheMatrix <- function(x = matrix()) {
+## This function will create a"matrix" object that will cache its inverse
 
+makeCacheMatrix <- function(x = matrix()) { ## default argument is set to "matrix"
+    inv <- NULL                             ## inv will hold the value of inverse of the matrix 
+    set <- function(y) {                   
+        x <<- y                             ## value of matrix in parent environment
+        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
+    }
+    get <- function() x                     ## define the get function - returns value of the matrix argument
+    
+    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
+    getinverse <- function() inv                     ## gets the value of inv where it is called
+    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## done in order to call the functions using $ operator 
+                                                                                 
 }
 
 
+## This function computes the inverse of the  "matrix" returned by makeCacheMatrix.
+## If the inverse has already been calculated (and the matrix has not changed),
+## then cacheSolve will retrieve the inverse from the cache.
 
 cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
+    inv <- x$getinverse()
+    if(!is.null(inv)) {
+        message("getting cached data")
+        return(inv)
+    }
+    data <- x$get()
+    inv <- solve(data, ...)
+    x$setinverse(inv)
+    inv
 }
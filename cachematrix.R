# usage: 
# somematrix <- matrix(rnorm(25), 5, 5) -- create 5x5 matrix of random data (must use square matrix to inverse it)
# v <- makeCacheMatrix(somematrix) -- turn a matrix into an object
# cacheSolve(v) -- either use object's internal functions to show solved inverse, or compute inverse and set it in the object

# pass any *square* matrix
# this object itself can be iterfaced with like so:
# o$set(somematrix) -- set the data 
# o$get() -- get the data from previous $set()
# o$setinv( solve(o$get()) ) -- set the inverse from running solve() on object's data
# o$getinv() -- return the inverse value if any
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # init the value of the inverse
  
  set <- function(y) {
    x <<- y # search parent envs for def
    i <<- NULL # search parent envs for def
  }
  
  # just return x
  get <- function() x
  
  # set to result of inverse passed here
  setinv <- function(inv) i <<- inv
  
  # return stored inverse if setinv() was called
  getinv <- function() i
  
  # return is a list of functions, but with data present?
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv
  )
}


# accepts an object returned by makeCacheMatrix()
# cacheSolve(m) -- take the m object and either use its internal functions to return computed inverse, or compute inverse and store it in object
cacheSolve <- function(x, ...) {
  # see if the solve() has already been stored in the object
  i <- x$getinv()
  
  # and return it if so
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # otherwise get the matrix from object
  data <- x$get()
  
  # compute inverse
  i <- solve(data, ...)
  
  # store inverse in the object
  x$setinv(i)
  
  # and return it
  i  
}

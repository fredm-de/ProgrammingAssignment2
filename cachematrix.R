## makeCacheMatrix can serve as a datastructure for a matrix and its cached inverse
## cacheSolve serves as a function to retrieve the inverse from cache if possible, otherwise calculated
## To use it, create an instance of makeCacheMatrix and use chacheSolve to get the inverse of the matrix


## makeCacheMatrix serves as a datastructure for saving a matrix and its inverse
## setters and getters are set(), get() for the matrix itself
## and setinverse(), getinverse() for the inverse inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    #set new matrix
    set <- function(matr){
      # if matr is different then x, set x to matr and delete the inverse
      if (sum(x == matr) == (ncol(matr)*nrow(matr)) && ncol(x) == ncol(matr) && nrow(x) == nrow(matr)){
        x <<- matr
        inv <<- NULL
      }
    }
    
    #return x
    get <- function(){
      x
    }
    
    #sets inverse to be new_inverse
    setinverse <- function(new_inverse){
      inverse <<- new_inverse
    }
    
    #returns inverse
    getinverse <- function(){
      inverse
    }
    
    #the functions return
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## CacheSolve returns cached inverse of x if existing, 
## otherwise calculates, caches and returns inverse of x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get cached inverse from x
  inverse <- x$getinverse()
  
  if (is.null(inverse)){
    #if inverse is empty calculate the inverse
    inverse <- solve(x$get())
    x$setinverse(inverse)
  } 
  
  #return inverse
  inverse
  
}

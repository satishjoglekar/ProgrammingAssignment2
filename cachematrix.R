## Takes matrix as input and computes it's Inverse
## Saves the Inverse in cache so it can be reused when required
## without having to recalculate the inverse each time it is required

## makeCacheMatrix Function takes the matrix as input function
## Returns a vector containg  pointers to functions, namely:
##      get(), set(), setInv() and getInv()

makeCacheMatrix <- function(x = matrix()) {

   # inv is in local in current env
   inv <- matrix()
   
   #sets the matrix data with the matrix argument
   set <- function(y) {
    #print("inside set function")
    # m here is being set in another env 
    m <<- y 
    # inv here is being set in another env
    inv <<- NULL
    #print("leaving set function")
    #print(m)
    }
    
  # return the matrix
  get <- function() {
    #print("in get function") 
    m
  }
  
  # sets the inverse
  setInv <- function(inverse){ 
    #print("In setInv function")
    inv <<- inverse
  }
  
  #returns the inverse
  getInv <- function(){
    #print("in getInv function") 
    inv
  }
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  

}


## cacheSolve function Computes the Matrix Inverse if not cached
## Else returns the cached value of the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # will return cached or calculated inverse
  #print("in function cacheSolve")
 
  inv<-x$getInv()
  
  if(!is.null(inv)){
    message("returning cached data")
    return(inv)
    
  }
  # reading Matrix!! in order to Calculate it's inverse"
  matrixData<-x$get()
  inv<-solve(matrixData)
  x$setInv(inv)
  inv
}

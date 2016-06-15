## makeCacheMatrix stores a matrix A in memory
## cacheSolve shows the inverse of a matrix if is in memory or computes 
the inverse and then shows the inverse

## Short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory

makeCacheMatrix <- function(A = matrix()) {
  INV <- NULL
  set <- function(B){
    A <<- B
    INV<<- NULL
  }
  get <- function() A
  setINV <- function(INV) INV<<- INV
  getINV <- function() INV
  list(set=set,get=get,setINV=setINV,getINV=getINV)
}

## Short comment describing this function
## cacheSolve uses corpcor, a library that avoids determinants and uses 
orthogonal descomposition
## note: this function will try to load corpcor library and if it's not 
installed will try to install the library

cacheSolve <- function(A, ...) 
{
  if(require("corpcor")){
    print("corpcor is loaded correctly")
  } else {
    print("trying to install corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor installed and loaded")
    } else {
      stop("could not install corpcor")
    }
  }
  INV<- A$getINV()
  if(!is.null(INV)){
    message("matrix is stored in memory")
    return(INV)
  }
  message("INV is not stored in memory so the INV (if exists) will be 
          computed")
  data <- A$get()
  INV <- pseudoinverse(data, ...)
  A$setINV(INV)
  INV
}


#Try if it works
#square matrix
A <- matrix(rpois(36,3), nrow = 6)
cA <- makeCacheMatrix(A)
cA$get()
cacheSolve(cA)
cacheSolve(cA)
invA <- cacheSolve(cA)

#Try if it works
#Rectangular matrix rows > cols
B <- matrix(rpois(56,2), nrow = 8, ncol = 7)
cB <- makeCacheMatrix(B)
cB$get()
cacheSolve(cB)
cacheSolve(cB)
invB <- cacheSolve(cB)

#Try if it works
#rectangular matrix rows < cols
C <- matrix(rpois(42,1), nrow = 3, ncol = 14)
cC <- makeCacheMatrix(C)
cC$get()
cacheSolve(cC)
cacheSolve(cC)
invC <- cacheSolve(cC)

#Try if it works
invA + A 
A + invA
invB + B 
C + invC 
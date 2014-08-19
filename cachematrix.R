## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL 
    #set the value of the vector
    set <- function(y) { 
        x <<- y 
        inv <<- NULL 
    } 
    #get the value of the vector	
    get <- function() x 

    #set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse 
    
    # get the value of the inverse
    getinverse <- function() inv 
    #cache the inverse of the matrix
    message ("Caching the inverse of the matrix")
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}




## Write a short comment describing this function
cacheSolve <- function(x, ...) { 


    #Check if the inverse is already computed
    inv <- x$getinverse() 

    # if the inverse is already computed return it from the cache
    if(!is.null(inv)) { 

 
        message("Getting Cached Data.") 

 
        return(inv) 

 
    } 

   # if the inverse is NOT already computed, solve for it 
    data <- x$get() 

 
    inv <- solve(data) 

 
    x$setinverse(inv) 

 
    inv 
}


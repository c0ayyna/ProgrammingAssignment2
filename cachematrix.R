## Put comments here that give an overall description of what your
## functions do
#The purpose of this function is to cache potentially time consuming operation of the inverse of a matix. 
## Write a short comment describing this function
#makeCacheMatrix createa a special object to store a numeric vector and cache its mean
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
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}




## Write a short comment describing this function
#the following function caculates the inverse of the special vector created in the funciton above - makeCacheMatrix
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


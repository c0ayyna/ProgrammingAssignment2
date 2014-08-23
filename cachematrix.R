#The purpose of this function is to cache potentially time consuming operation of the inverse of a matix and use it from the cache, rather than compute it every time it is needed.
#makeCacheMatrix creates a special object. This special object is a list of following four functions :
# set - to set the value of the matrix, 
# get - te get the value of the matirx,
# setinverse - to set the value of the inverse of the matrix.
# getinverse - to get the value of the inverse of the matirx.
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL 
    #set the value of the  matrix
    set <- function(y) { 
        x <<- y 
        inv <<- NULL 
    } 
    #get the value of the matrix	
    get <- function() x 

    #set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse 
    
    # get the value of the inverse
    getinverse <- function() inv 

    # return a list of the functions - set, get,setinverse and get inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}

#cacheSolve calculates the inverse of the special vector created in the funciton above - makeCacheMatrix
cacheSolve <- function(x, ...) { 
    #Check if the inverse is already computed 
    inv <- x$getinverse()  # get the inverse and assign it to inv
    # if the inverse is already computed i.e NOT NULL,
    #  return the inverse from the cache  
    if(!is.null(inv)) { 
        message("Getting Cached Data.") 
        return(inv) 
 
    } 

   # if the inverse is NOT already computed, solve for it
    data <- x$get()     # assign the matrix to data
    inv <- solve(data)  # Solve for the inverse using solve function
    x$setinverse(inv)   # Set the inverse in cache
    inv                 # return the inverse of the matrix

}

